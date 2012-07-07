# replace `=` by `<-` in expressions
replace_assignment = function(exp) {
  library(codetools)
  wc = makeCodeWalker(
    call = function (e, w) {
      cl = walkCode(e[[1]], w)
      arg = lapply(as.list(e[-1]), function(a) if (missing(a)) NA else walkCode(a, w))
      as.call(c(list(cl), arg))
    },
    leaf = function(e, w) {
      if (length(e) == 0 || inherits(e, "srcref")) return(NULL)
      # x = 1 is actually `=`(x, 1), i.e. `=` is a function
      if (identical(e, as.name("="))) e <- as.name("<-")
      e
    })
  lapply(as.list(exp), walkCode, w = wc)
}

## replace inline comments to cheat R

# a literal # must be writen in double quotes, e.g. "# is not comment"
mask_inline = function(x) {
  # move comments after { to the next line
  if (length(idx <- grep('\\{\\s*#.*$', x))) {
    p = paste('{\ninvisible("', begin.comment, '\\1', end.comment, '")', sep = '')
    x[idx] = gsub('\\{\\s*(#.*)$', p, x[idx])
  }
  gsub('(#[^"]*)$', ' %InLiNe_IdEnTiFiEr% "\\1"', x)
}

# reflow comments (including roxygen comments)
reflow_comments = function(text, idx = grepl('^\\s*#+', text), width = getOption('width')) {
  r = rle(idx)$lengths; flag = idx[1] # code and comments alternate in text
  unlist(lapply(split(text, rep(seq(length(r)), r)), function(x) {
    if (flag) {
      b = sub("^\\s*(#+)('?).*", '\\1\\2 ', x[1])
      x = paste(b, paste(gsub("^\\s*(#+)('?)", '', x), collapse = '\n'))
      x = strwrap(x, width = width, prefix = b, initial = '')
    }
    flag <<- !flag
    x
  }), use.names = FALSE)
}

# reindent lines with a different number of spaces
reindent_lines = function(text, n = 2) {
  if (n == 4) return(text)  # no need to do anything
  s = paste(rep(' ', n), collapse = '')
  unlist(lapply(strsplit(text, '\n', fixed = TRUE), function(x) {
    t1 = gsub('^( *)(.*)', '\\1', x)
    t2 = gsub('^( *)(.*)', '\\2', x)
    paste(gsub(' {4}', s, t1), t2, sep = '', collapse = '\n')
  }), use.names = FALSE)
}

# text is the code, n is number of spaces for indentation
move_leftbrace = function(text, n) {
  s = paste(rep(' ', n), collapse = '')
  unlist(lapply(strsplit(text, '\n', fixed = TRUE), function(x) {
    if (!length(idx <- grep('(\\)|else) \\{$', x))) return(x)
    # remove first n spaces from the next lines, and use this amount of spaces
    # for the { lines
    pre = substring(gsub('^( *)(.*)', '\\1', x[idx + 1L]), n + 1L)
    x[idx] = mapply(gsub, '(\\)|else) \\{$', paste('\\1\n', pre, '{', sep = ''), x[idx],
                    USE.NAMES = FALSE)
    x
  }), use.names = FALSE)
}
