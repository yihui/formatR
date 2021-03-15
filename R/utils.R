# replace `=` by `<-` in expressions
replace_assignment = function(exp) {
  wc = codetools::makeCodeWalker(
    call = function(e, w) {
      cl = codetools::walkCode(e[[1]], w)
      arg = lapply(as.list(e[-1]), function(a) if (missing(a)) NA else {
        codetools::walkCode(a, w)
      })
      as.call(c(list(cl), arg))
    },
    leaf = function(e, w) {
      if (length(e) == 0 || inherits(e, "srcref")) return(NULL)
      # x = 1 is actually `=`(x, 1), i.e. `=` is a function
      if (identical(e, as.name("="))) e <- as.name("<-")
      e
    })
  lapply(as.list(exp), codetools::walkCode, w = wc)
}

## mask comments to cheat R
mask_comments = function(x, width, keep.blank.line) {
  d = utils::getParseData(parse_source(x))
  if (nrow(d) == 0 || (n <- sum(d$terminal)) == 0) return(x)
  d = d[d$terminal, ]
  d = fix_parse_data(d, x)
  d.line = d$line1; d.line2 = d$line2; d.token = d$token; d.text = d$text

  # move else back
  for (i in which(d.token == 'ELSE')) {
    delta = d.line[i] - d.line[i - 1]
    d.line[i:n] = d.line[i:n] - delta
    d.line2[i:n] = d.line2[i:n] - delta
  }
  # how many blank lines after each token?
  blank = c(pmax(d.line[-1] - d.line2[-n] - 1, 0), 0)

  i = d.token == 'COMMENT'
  # double backslashes and replace " with ' in comments
  d.text[i] = gsub('"', "'", gsub('\\\\', '\\\\\\\\', d.text[i]))

  c0 = d.line[-1] != d.line[-n]  # is there a line change?
  c1 = i & c(TRUE, c0 | (d.token[-n] == "'{'"))  # must be comment blocks
  c2 = i & !c1  # inline comments
  c3 = c1 & grepl("^#+[-'+]", d.text)  # roxygen or knitr spin() comments
  if (grepl('^#!', d.text[1])) c3[1] = TRUE  # shebang comment

  # reflow blocks of comments: first collapse them, then wrap them
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
  d.text[c1 & !c3] = reflow_comments(d.text[c1 & !c3], width)
  d.text[c3] = sprintf('invisible("%s%s%s")', begin.comment, d.text[c3], end.comment)
  d.text[c2] = sprintf('%%InLiNe_IdEnTiFiEr%% "%s"', d.text[c2])

  # add blank lines
  if (keep.blank.line) for (i in seq_along(d.text)) {
    if (blank[i] > 0)
      d.text[i] = paste(c(d.text[i], rep(blank.comment, blank[i])), collapse = '\n')
  }

  unlist(lapply(split(d.text, d.line), paste, collapse = ' '), use.names = FALSE)
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

# a literal # must be writen in double quotes, e.g. "# is not comment"
mask_inline = function(x) {
  # move comments after { to the next line
  if (length(idx <- grep('\\{\\s*#.*$', x))) {
    p = paste('{\ninvisible("', begin.comment, '\\1', end.comment, '")', sep = '')
    x[idx] = gsub('\\{\\s*(#.*)$', p, x[idx])
  }
  gsub('(#[^"]*)$', ' %InLiNe_IdEnTiFiEr% "\\1"', x)
}

# reflow comments (excluding roxygen comments)
reflow_comments = function(x, width) {
  if (length(x) == 0) return(x)
  # returns a character vector of the same length as x
  b = sub('^(#+).*', '\\1', x)
  mapply(function(res, prefix) {
    paste(sprintf(
      'invisible("%s%s%s")', begin.comment, paste(prefix, res), end.comment
    ), collapse = '\n')
  }, strwrap(sub('^#+', '', x), width = width, simplify = FALSE), b)
}

# reindent lines with a different number of spaces
reindent_lines = function(text, n = 2) {
  if (length(text) == 0) return(text)
  if (n == 4) return(text)  # no need to do anything
  s = paste(rep(' ', n), collapse = '')
  unlist(lapply(strsplit(text, '\n'), function(x) {
    t1 = gsub('^( *)(.*)', '\\1', x)
    t2 = gsub('^( *)(.*)', '\\2', x)
    paste(gsub(' {4}', s, t1), t2, sep = '', collapse = '\n')
  }), use.names = FALSE)
}

# move { to the next line
move_leftbrace = function(text) {
  if (!length(text)) return(text)
  # the reason to use lapply() here is that text is a vector of source code with
  # each element being a complete R expression; we do not want to break the
  # expression structure; same reason for reindent_lines() above
  unlist(lapply(strsplit(text, '\n'), function(x) {
    if (length(x) > 1L && length(idx <- grep('(\\)|else) \\{$', x))) {
      # indent the same amount of spaces as the { lines
      pre = gsub('^( *)(.*)', '\\1', x[idx])
      x[idx] = mapply(gsub, '(\\)|else) \\{$', sprintf('\\1\n%s{', pre), x[idx],
                      USE.NAMES = FALSE)
    }
    paste(x, collapse = '\n')
  }), use.names = FALSE)
}

# parse but do not keep source (moved from knitr)
parse_only = function(code) {
  if (length(code) == 0) return(expression())
  base::parse(text = code, keep.source = FALSE)
}

# copied from highr
# TODO: eventually remove the hack for R <= 3.2.2
parse_source = if (getRversion() > '3.2.2') function(lines) {
  parse(text = lines, keep.source = TRUE)
} else function(lines) {
  # adapted from evaluate
  src = srcfilecopy('<text>', lines = '')
  if (length(grep('\n', lines))) lines = unlist(strsplit(
    sub('$', '\n', as.character(lines)), '\n'
  ))
  src$lines = lines
  parse(text = lines, srcfile = src)
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
  paste(x[l1:l2], collapse = '\n')
}

# generate a random string
CHARS = c(letters, LETTERS, 0:9)
rand_string = function(len = 32) {
  paste(sample(CHARS, len, replace = TRUE), collapse = '')
}

.env = new.env()
.env$line_break = NULL

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

trimws = function(x, which = c('both', 'left', 'right')) {
  switch(match.arg(which),
    both = gsub('^\\s+|\\s+$', '', x),
    left = gsub('^\\s+', '', x), right = gsub('\\s+$', '', x)
  )
}
