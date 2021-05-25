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
assert('keep.blank.line=FALSE removes blank lines', {
  (tidy.res(x1) %==% c('1 + 1', '', 'if (F) {\n\n}', ''))
  (tidy.res(x1, blank = FALSE) %==% c('1 + 1', 'if (F) {\n}'))
})

assert('= can be replaced with <- when replace.assign=TRUE', {
  (tidy.res('x=1;c(x=1)', arrow = TRUE) %==% c('x <- 1', 'c(x = 1)'))
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
