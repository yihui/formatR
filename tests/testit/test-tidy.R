library(testit)

tidy.res = function(x, ...) {
  tidy_source(text = x, ..., output = FALSE)$text.tidy
}

assert('tidy_source() tries to keep R comments', {
  (tidy.res('1+1#asdf') %==% '1 + 1  #asdf')
  (tidy.res('paste(1 #asdf\n,2)') %==% 'paste(1  #asdf\n, 2)')
  (tidy.res(c('# asdf', '1+1')) %==% c('# asdf', '1 + 1'))
})

assert('tidy_source() preserves backslashes in comments', {
  (tidy.res('# \\a \\b \\c') %==% '# \\a \\b \\c')
})

assert('tidy_source() can preserve blank lines among non-empty code lines', {
  (tidy.res(c('if(TRUE){1+1', '', '}', '', '# a comment')) %==%
     c('if (TRUE) {\n    1 + 1\n\n}', '', '# a comment'))
})

x1 = paste(c('#', letters), collapse = ' ')
x2 = c('# a b c d e f g h i', '# j k l m n o p q r', '# s t u v w x y z')
assert('long comments are wrapped in tidy_source()', {
  (tidy.res(x1, width.cutoff = 20) %==% one_string(x2))
  (tidy.res(rep(x1, 2), width.cutoff = 20) %==%
      '# a b c d e f g h i
# j k l m n o p q r
# s t u v w x y z a
# b c d e f g h i j
# k l m n o p q r s
# t u v w x y z'
  )
  (tidy.res(c(x1, '1+1', x1), width.cutoff = 20) %==%
      c(one_string(x2), '1 + 1', one_string(x2)))
})

assert('roxygen comments are not wrapped', {
  (tidy.res(c(paste("#'", x1), '1*1')) %==% c(paste("#'", x1), '1 * 1'))
})

assert('wrap = FALSE does not wrap long comments', {
  (tidy.res(x1, width.cutoff = 20, wrap = FALSE) %==% x1)
})

x1 = '
# only a comment
'
x2 = c('', '# only a comment', '', '')
assert('tidy_source() can deal with code that only contains a comment', {
  (tidy.res(x1) %==% c('', '# only a comment', ''))
  (tidy.res(x2) %==% x2)
})

assert('tidy_source() works for empty comments', {
  (tidy.res('#') %==% '#')
  (tidy.res(c('#', 'a+b')) %==% c('#', 'a + b'))
})

x1 = '{if (TRUE) {
1
}
else 2}'
assert('tidy_source() moves else back if it is in a standalone line', {
  (tidy.res(x1) %==% '{\n    if (TRUE) {\n        1\n    } else 2\n}')
})

x1 = '{x=1
else.x=2
}'

assert('should not move any lines starting with `else` back to the previous line', {
  (tidy.res(x1) %==% '{\n    x = 1\n    else.x = 2\n}')
})

x1 = 'if (TRUE) {# comment
1
}'
assert('comments after { are moved down one line', {
  (tidy.res(x1) %==% 'if (TRUE) {\n    # comment\n    1\n}')
})

assert('empty code returns empty string', {
  (tidy.res('') %==% '')
  (tidy.res(c('', '  ')) %==% c('', '  '))
})

assert('keep.comment=FALSE removes comments', {
  (tidy.res(c('# a comment', '1+1'), comment = FALSE) %==%
     '1 + 1')
})

assert('when comment=FALSE and everything is comment, tidy_source() returns character(0)', {
  (tidy.res('# a comment', comment = FALSE) %==% character(0))
})

x1 = '1+1

if(F){

}
'
assert('blank=FALSE removes blank lines', {
  (tidy.res(x1) %==% c('1 + 1', '', 'if (F) {\n\n}', ''))
  (tidy.res(x1, blank = FALSE) %==% c('1 + 1', 'if (F) {\n}'))
})

assert('The assignment operator = can be replaced with <- when arrow = TRUE', {
  (tidy.res('x=1;c(x=1) # abc', arrow = TRUE) %==% c('x <- 1', 'c(x = 1)  # abc'))
  (tidy.res('x=1;c(x=1) # abc', arrow = TRUE, comment=FALSE) %==% c('x <- 1', 'c(x = 1)'))
})

assert('since R 3.0.0 comments can be written with double quotes in them', {
  (tidy.res('1+1# hello "world"') %==% "1 + 1  # hello 'world'")
})

x1 = 'x="
# this is not a comment
"'
assert('since R 3.0.0, # in the beginning of a line does not necessarily mean comments', {
  (tidy.res(x1) %==% 'x = "\n# this is not a comment\n"')
})

assert('the shebang is preserved', {
  (tidy.res(c('#!/usr/bin/Rscript', '1+1')) %==% c('#!/usr/bin/Rscript', '1 + 1'))
})

x1 = paste0('x="', r <- rand_string(2000), '"')
assert('Long strings (> 1000 chars) can be preserved', {
  (tidy.res(x1) %==% paste0('x = "', r, '"'))
})

x1 = 'x = "
  this is a
  character string
"'
assert('line breaks in strings are preserved instead of being replaced by \\n', {
  (tidy.res(x1) %==% x1)
})

# tests for magrittr newlines

x1 = '
iris %>% group_by(Species) %>%
summarize(meanlen = mean(Sepal.Length)) %$% arrange(meanlen) %>%meanlen
'

x2 = '
iris %>%
  group_by(Species) %>%
  summarize(meanlen = mean(Sepal.Length)) %$%
  arrange(meanlen) %>%
  meanlen
'

assert('magrittr lines are wrapped after the pipes', {
  (one_string(tidy.res(x1, indent = 2)) %==% x2)
})

if (getRversion() >= '4.1.0') assert('The new pipe |> is supported', {
  (tidy.res('1|>c()') %==% '1 |>\n    c()')
})

assert('The magrittr pipe can be substituted with the base R pipe', {
  (tidy.res('1%>%c()%>%paste()', pipe = TRUE) %==% '1 |>\n    c() |>\n    paste()')
})

assert('The right arrow -> assignment operator is supported', {
  (tidy.res('1->a# right assign') %==% '1 -> a  # right assign')
})

assert('args.newline = TRUE can start function arguments on a new line', {
  x1 = 'c(aaaaa=1,bbbbb=2,ccccc=3,ddddd=4)'
  (tidy.res(x1, args.newline = TRUE, width.cutoff = 20) %==%
     'c(\n    aaaaa = 1, bbbbb = 2,\n    ccccc = 3, ddddd = 4\n)')
  (tidy.res(x1, args.newline = TRUE, width.cutoff = 40) %==%
      'c(\n    aaaaa = 1, bbbbb = 2, ccccc = 3, ddddd = 4\n)')
  # strict width
  (tidy.res(x1, args.newline = TRUE, width.cutoff = I(40)) %==%
      'c(\n    aaaaa = 1, bbbbb = 2, ccccc = 3,\n    ddddd = 4\n)')
  (tidy.res(x1, args.newline = TRUE, width.cutoff = I(23), indent = 2) %==%
      'c(\n  aaaaa = 1, bbbbb = 2,\n  ccccc = 3, ddddd = 4\n)')
  # when arguments can fit one line, don't break the line after function name
  (tidy.res(x1, args.newline = TRUE, width.cutoff = 45) %==%
      'c(aaaaa = 1, bbbbb = 2, ccccc = 3, ddddd = 4)')
  # nested calls
  x2 = 'lm(y~x1+x2+x3+x4+x5+x6+x7+x8, data=data.frame(y=rnorm(100),x1=rnorm(100),x2=rnorm(100)))'
  (tidy.res(x2, args.newline = TRUE, width.cutoff = 20, indent = 2) %==%
      'lm(
  y ~ x1 + x2 + x3 +
    x4 + x5 + x6 +
    x7 + x8, data = data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
)')
  (tidy.res(x2, args.newline = TRUE, width.cutoff = I(25), indent = 2) %==%
      'lm(
  y ~ x1 + x2 + x3 + x4 +
    x5 + x6 + x7 + x8,
  data = data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
)')
  # also works on function() definitions in addition to function calls
  x3 = 'my_sum=function(a=1,b=2,c=3,d=4,e=5,f=6,g=7){return(a+b+c)}'
  (tidy.res(x3, args.newline = TRUE, width.cutoff = 20, indent = 2) %==%
      'my_sum = function(\n  a = 1, b = 2, c = 3,\n  d = 4, e = 5, f = 6,\n  g = 7\n) {\n  return(a + b + c)\n}')
  (tidy.res(x3, args.newline = TRUE, width.cutoff = I(33), indent = 2) %==%
      'my_sum = function(\n  a = 1, b = 2, c = 3, d = 4,\n  e = 5, f = 6, g = 7\n) {\n  return(a + b + c)\n}')
})
