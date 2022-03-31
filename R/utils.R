parse_data = function(x) {
  d = utils::getParseData(parse_source(x, TRUE))
  d[d$terminal, ]
}

# mask comments in strings so that deparse() will not drop them
mask_comments = function(x, comment, blank.line, wrap, arrow, pipe, args.newline, spaces) {
  d = parse_data(x)
  if ((n <- nrow(d)) == 0) return(x)
  d = fix_parse_data(d, x)
  if (args.newline) d = insert_arg_breaks(d, spaces)
  d.line = d$line1; d.line2 = d$line2; d.token = d$token; d.text = d$text

  # move else back
  for (i in which(d.token == 'ELSE')) {
    delta = d.line[i] - d.line[i - 1]
    d.line[i:n] = d.line[i:n] - delta
    d.line2[i:n] = d.line2[i:n] - delta
  }
  # how many blank lines after each token?
  blank = c(pmax(d.line[-1] - d.line2[-n] - 1, 0), 0)

  # substitute = with <- when = means assignment
  if (arrow) d.text[d.token == 'EQ_ASSIGN'] = '<-'
  # substitute %>% with |>
  if (pipe) d.text[d.token == 'SPECIAL' & d.text == '%>%'] = '|>'

  i = if (comment) d.token == 'COMMENT' else logical(n)
  # double backslashes and substitute " with ' in comments
  d.text[i] = gsub('"', "'", gsub('\\\\', '\\\\\\\\', d.text[i]))

  c0 = d.line[-1] != d.line[-n]  # is there a line change?
  c1 = i & c(TRUE, c0 | (d.token[-n] == "'{'"))  # must be comment blocks
  c2 = i & !c1  # inline comments
  c3 = c1 & grepl("^#+[-'+]", d.text)  # roxygen or knitr spin() comments
  if (wrap) {
    if (grepl('^#!', d.text[1])) c3[1] = TRUE  # shebang comment
  } else c3 = c1  # comments not to be wrapped

  # collapse blocks of comments
  i1 = which(c1 & !c3) # do not wrap roxygen comments
  j1 = i1[1]
  if (length(i1) > 1) for (i in 2:length(i1)) {
    # two neighbor lines of comments
    if (d.line[i1[i]] - d.line[i1[i - 1]] == 1) {
      j2 = i1[i]
      d.text[j1] = paste(d.text[j1], sub('^#+', '', d.text[j2]))
      d.text[j2] = ''
      c1[j2] = FALSE  # the second line is no longer a comment
    } else j1 = i1[i]
  }

  # mask block and inline comments
  d.text[c1] = sprintf('invisible("%s%s%s")', begin.comment, d.text[c1], end.comment)
  d.text[c2] = sprintf('%%\b%% "%s"', d.text[c2])

  # add blank lines
  if (blank.line) for (i in seq_along(d.text)) {
    if (blank[i] > 0)
      d.text[i] = one_string(c(d.text[i], rep(blank.comment, blank[i])))
  }
  # break lines after some infix operators such as %>%
  d.text = gsub(paste0('^(%)(', infix_ops, ')(%)$'), paste0('\\1\b\\2', spaces, '\\3'), d.text)
  # similarly break lines after |>; later restore %\b|>% to |> in unmask_source()
  d.text[d.text == '|>'] = paste0('%', '\b|>', spaces, '%')
  # preserve the assignment operator ->
  d.text[d.text == '->'] = '%\b->%'

  unlist(lapply(split(d.text, d.line), paste, collapse = ' '), use.names = FALSE)
}

infix_ops = '[>$]|T>|<>'

restore_infix = function(x) {
  x = gsub('%\b([^ %]+)%', '\\1', x)
  x = gsub('%\b([|]>) +%\\s*', '\\1', x)
  x = gsub('(%)\b([^ ]+) +(%)\\s*$', '\\1\\2\\3', x)
  x
}

# no blank lines before an 'else' statement!
move_else = function(x) {
  blank = grepl('^\\s*$', x)
  if (!any(blank)) return(x)
  else.line = grep('^\\s*else(\\s+|$)', x)
  for (i in else.line) {
    j = i - 1
    while (blank[j]) {
      blank[j] = FALSE; j = j - 1  # search backwards & rm blank lines
      warning('removed blank line ', j, " (should not put an 'else' in a separate line!)")
    }
  }
  x[blank] = blank.comment
  x
}

# reflow comments (excluding roxygen comments)
reflow_comments = function(x, width, wrap) {
  c1 = grepl(mat.comment, x)
  x  = gsub(pat.comment, '', x)  # strip the invisible() masks
  if (!wrap) return(x)
  x[c1] = restore_bs(x[c1])
  c2 = c1 & !grepl("^\\s*#+[-'+!]", x)
  # extract indent & comment prefix, e.g., '## '
  r  = '^(\\s*#+)\\s*(.*)'
  x[c2] = unlist(lapply(x[c2], function(z) {
    p = sub(r, '\\1', z)
    z = sub(r, '\\2', z)
    z = one_string(strwrap(z, width, prefix = paste0(p, ' ')))
    # the comment might be empty, in which case we return the comment chars
    if (z == '') p else z
  }))
  x
}

# reindent lines with a different number of spaces
reindent_lines = function(text, spaces = rep_chars(n), n = 2) {
  if (length(text) == 0) return(text)
  if (spaces == '    ') return(text)  # no need to do anything
  t1 = gsub('^( *)(.*)', '\\1', text)
  t2 = gsub('^( *)(.*)', '\\2', text)
  paste0(gsub(' {4}', spaces, t1), t2)
}

# move { to the next line
move_leftbrace = function(x) {
  if (length(idx <- grep('(\\)|else) \\{$', x)) == 0) return(x)
  # indent the same amount of spaces as the { lines
  b = gsub('^( *)(.*)', '\\1{', x[idx])
  j = 0
  x[idx] = gsub(' \\{$', '', x[idx])
  for (i in seq_along(idx)) {
    x = append(x, b[i], idx[i] + j)
    j = j + 1
  }
  x
}

# base::parse(text = NULL) will ask for user input but should return an empty
# expression instead
parse_source = function(code, keep.source = FALSE) {
  if (length(code) == 0) return(expression())
  base::parse(text = code, keep.source = keep.source)
}

# restore backslashes
restore_bs = function(x) gsub('\\\\\\\\', '\\\\', x)

# a workaround for the R bug (long strings are truncated in getParseData()):
# https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16354
fix_parse_data = function(d, x) {
  if (length(s <- which(d$token == 'STR_CONST')) == 0) return(d)
  ws = s[grep('^\\[\\d+ (wide )?chars quoted with \'"\'\\]$', d$text[s])]
  for (i in ws) {
    di = d[i, , drop = FALSE]
    d[i, 'text'] = get_src_string(x, di$line1, di$line2, di$col1, di$col2)
  }
  d[s, 'text'] = mask_line_break(d[s, 'text'])
  d
}

get_src_string = function(x, l1, l2, c1, c2) {
  if (l1 == l2) return(substr(x[l1], c1, c2))
  x[l1] = substr(x[l1], c1, nchar(x[l1]))
  x[l2] = substr(x[l2], 1, c2)
  one_string(x[l1:l2])
}

# generate a random string
CHARS = c(letters, LETTERS, 0:9)
rand_string = function(len = 32) {
  paste(sample(CHARS, len, replace = TRUE), collapse = '')
}

.env = new.env()
.env$line_break = NULL

# protect \n in source code, otherwise deparse() will change it to \\n
mask_line_break = function(x) {
  if (length(grep('\n', x)) == 0) return(x)
  m = (function() {
    for (i in 2:10) {
      for (j in 1:100) if (length(grep(s <- rand_string(i), x)) == 0) return(s)
    }
  })()
  if (is.null(m)) return(x)
  .env$line_break = m
  gsub('\n', m, x)
}

# add a long argument to a function call, so that other arguments can be pushed
# to the next line; this is for breaking arguments onto new lines, e.g.,
# c(a = 1) -> c(`\b    \b`, a = 1, `\b    \b#`) ->
# c(`\b    \b`,
#   a = 1,
#   `\b    \b#`)
# ->
# c(
#   a = 1
# )
insert_arg_breaks = function(d, spaces) {
  if (length(i <- which(d$token %in% c('SYMBOL_FUNCTION_CALL', 'FUNCTION'))) == 0)
    return(d)
  i1 = i[d[i + 1, 'token'] == "'('"] + 1  # the next line must be (
  i1 = i1[d[i1 + 1, 'token'] != "')'"]  # there must be arguments inside ()
  if (length(i1) == 0) return(d)
  i2 = which(d$token == "')'")
  i2 = i2[d[i2, 'parent'] %in% d[i1, 'parent']]  # ) that shares same parent as (
  if (length(i1) != length(i2)) {
    warning('( and ) do not match in function calls.')
    return(d)
  }
  s1 = arg_spaces(spaces, d[i1, 'parent'])
  s2 = arg_spaces(spaces, d[i2, 'parent'], '#')
  s3 = gsub('#', '##', s2)
  d[i1, 'text'] = paste0('(', s1, ',')
  d[i2, 'text'] = paste0(',', s2, ',', s3, ')')
  d
}

arg_spaces = function(x, id, id2 = '') sprintf('`%s\b%s\b%s`', id, x, id2)

# restore breaks for all function calls
restore_arg_breaks = function(
  x, width, spaces = rep_chars(width), indent = '    ', split = FALSE
) {
  s = gsub('\b', '\\\\\\\\b', arg_spaces(spaces, '([0-9]+)', '#{0,2}'))
  if (length(grep(s, x)) == 0) return(x)
  if (split) x = one_string(x)
  m = gregexpr(s, x)
  id = gsub(s, '\\1', unlist(regmatches(x, m)))
  for (i in sort(as.integer(unique(id)))) {
    x = restore_arg_break(x, i, s, width, indent)
  }
  if (split) x = split_lines(x)
  x
}

# restore one function call
restore_arg_break = function(x, i, s, width, indent) {
  s2 = paste0(s, '(,\\s*)?')
  t = sub('([0-9]+)', i, s, fixed = TRUE)
  r = sprintf('(\\()%s,\\s*?(\n\\s*)(.*?)\\s*,\\s*%s,\\s*?(\n\\s*)%s(\\))', t, t, t)
  m = gregexpr(r, x)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    if (length(z) == 0) return(z)
    # first try not to move arguments onto new lines and check if the code
    # exceeds the desired width
    x1 = gsub(r, '\\1\\3\\5', z)
    # if all arguments fit one line, just put them on one line
    if (!grepl('\n', x1) &&
        !any(exceed_width(split_lines(gsub(s2, '', x1)), width))) return(x1)
    x1 = gsub(r, '\\1\\2\\3\\4\\5', z)
    # indent ) back one level
    sub(sprintf('(\n)%s(\\s*\\))$', indent), '\\1\\2', x1)
  })
  x
}

exceed_width = function(x, width) nchar(x, type = 'width') > width

split_lines = function(x) unlist(strsplit(x, '\n'))

rep_chars = function(width, char = ' ') paste(rep(char, width), collapse = '')

trimws = function(x, which = c('both', 'left', 'right')) {
  switch(match.arg(which),
    both = gsub('^\\s+|\\s+$', '', x),
    left = gsub('^\\s+', '', x), right = gsub('\\s+$', '', x)
  )
}

one_string = function(..., collapse = '\n') paste(..., collapse = collapse)

restore_encoding = function(x, enc) {
  if (length(enc) != 1) return(x)
  xenc = special_encoding(x)
  iconv(x, if (length(xenc) == 0) '' else xenc, enc)
}

special_encoding = function(x) setdiff(Encoding(x), 'unknown')
