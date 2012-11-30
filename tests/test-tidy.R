library(formatR)
tidy.res = function(x, ...) {
  tidy.source(text = x, ..., output = FALSE)$text.tidy
}

stopifnot(
  identical(tidy.res('1+1#asdf'), '1 + 1  #asdf'),
  identical(tidy.res('paste(1 #asdf\n,2)'), 'paste(1  #asdf\n, 2)'),
  identical(tidy.res(c('# asdf', '1+1')), c('# asdf', '1 + 1'))
)
