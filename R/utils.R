# faster than require() but less rigorous
has_package = function(pkg) pkg %in% .packages(TRUE)

## replace inline comments to cheat R

# rules: if you do not have parser installed, a literal # must be writen in
# double quotes, e.g. "# is not comment"; = for assignment must stand in its own
# line, and one line per assignment
mask.inline = function(x, replace.assign, begin.comment, end.comment) {
  if (!has_package('parser')) {
    # move comments after { to the next line
    if (length(idx <- grep('\\{\\s*#.*$', x))) {
      p = paste('{\ninvisible("', begin.comment, '\\1', end.comment, '")', sep = '')
      x[idx] = gsub('\\{\\s*(#.*)$', p, x[idx])
    }
    if (replace.assign) {
      warning('replace.assign=TRUE may not be reliable without the parser package!')
      x = gsub('^(\\s*[[:alnum:]_\\.]+\\s*)=(\\s*[^,]+)$', '\\1 <- \\2', x)
    }
    return(gsub('(#[^"]*)$', ' %InLiNe_IdEnTiFiEr% "\\1"', x))
  }
  # use the parser package to deal with = and inline comments
  parser = getFromNamespace('parser', 'parser')
  enc = options(encoding = "native.enc")
  out = try(attr(parser(text = x), 'data'), silent = TRUE)
  options(enc)
  if (inherits(out, 'try-error')) {
    m = seq_along(x)
    ## line number where errors occur
    n = as.numeric(tail(strsplit(strsplit(out, '\n')[[1]][2], ':')[[1]], 2)[1])
    if (n > length(m)) n = length(m)
    r = (-3:3) + m[n]; r = r[r > 0 & r <= length(text)]
    s = paste(rep('#', .75 * getOption('width')), collapse = '')
    message('Unable to parse the R code! ',
            'The error most likely came from line ', m[n],
            '; \nthe surrounding lines are:\n', s, '\n',
            paste(text[r], collapse = '\n'), '\n', s, '\n',
            'See the reference in help(tidy.source) for possible reasons',
            '\n')
    stop(out)
  }
  out = subset(out, out$terminal)
  if (nrow(out) == 0) return(x) # nothing here
  
  if (replace.assign) {
    out$text[out$token.desc=='EQ_ASSIGN'] = '<-'
  }
  ## is inline comment?
  idx1 = c(FALSE, diff(out$line1)==0) & (out$token.desc=='COMMENT')
  ## is last line '{'?
  idx2 = c(FALSE, (out$text == '{')[-length(idx1)])
  out$text[idx1] = gsub('"', "'", out$text[idx1])
  idx = idx1 & (!idx2)
  out$text[idx] = sprintf(' %%InLiNe_IdEnTiFiEr%% "%s"', out$text[idx])
  idx = idx1 & idx2
  out$text[idx] = sprintf('invisible("%s%s%s")', begin.comment, out$text[idx], end.comment)
  tapply(out$text, out$line1, paste, collapse=' ')
}

# reflow comments (including roxygen comments)
reflow.comments = function(text, idx = grepl('^\\s*#+', text), width = getOption('width')) {
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
reindent.lines = function(text, n = 2) {
  if (n == 4) return(text)  # no need to do anything
  s = paste(rep(' ', n), collapse = '')
  t1 = gsub('^( *)(.*)', '\\1', text)
  t2 = gsub('^( *)(.*)', '\\2', text)
  paste(gsub(' {4}', s, t1), t2, sep = '')
}
