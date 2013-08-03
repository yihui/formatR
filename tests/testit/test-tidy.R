library(testit)

tidy.res = function(x, ...) {
  tidy.source(text = x, ..., output = FALSE)$text.tidy
}

assert(
  'tidy.source() tries to keep R comments',
  identical(tidy.res('1+1#asdf'), '1 + 1  #asdf'),
  identical(tidy.res('paste(1 #asdf\n,2)'), 'paste(1  #asdf\n, 2)'),
  identical(tidy.res(c('# asdf', '1+1')), c('# asdf', '1 + 1'))
)

assert(
  'tidy.source() preserves backslashes in comments',
  identical(tidy.res('# \\a \\b \\c'), '# \\a \\b \\c')
)

assert(
  'tidy.source() can preserve blank lines among non-empty code lines',
  identical(tidy.res(c('if(TRUE){1+1', '', '}', '', '# a comment')),
            c('if (TRUE) {\n    1 + 1\n    \n}', '', '# a comment'))
)

x1 = paste(c('#', letters), collapse = ' ')
x2 = c('# a b c d e f g h i j', '# k l m n o p q r s t', '# u v w x y z')
if (R3) assert(
  'long comments are wrapped in tidy.source()',
  identical(tidy.res(x1, width.cutoff = 20), x2),
  identical(
    tidy.res(rep(x1, 2), width.cutoff = 20),
    c('# a b c d e f g h i j', '# k l m n o p q r s t', '# u v w x y z a b c d',
      '# e f g h i j k l m n', '# o p q r s t u v w x', '# y z')
  ),
  identical(tidy.res(c(x1, '1+1', x1), width.cutoff = 20), c(x2, '1 + 1', x2))
)
if (R3) assert(
  'roxygen comments are not wrapped',
  identical(tidy.res(c(paste("#'", x1), '1*1')), c(paste("#'", x1), '1 * 1'))
)

x1 = '
# only a comment
'
x2 = c('', '#only a comment', '', '')
if (R3) assert(
  'tidy.source() can deal with code that only contains a comment',
  identical(tidy.res(x1), '# only a comment'),
  identical(tidy.res(x2), '# only a comment')
)

x1 = '{if (TRUE) {
1
}
else 2}
'
assert(
  'tidy.source() moves else back if it is in a standalone line',
  identical(tidy.res(x1), '{\n    if (TRUE) {\n        1\n    } else 2\n}')
)

if (packageVersion('formatR') > '0.8') assert(
  'when keep.comment=FALSE and everything is comment, tidy.source() returns character(0)',
  identical(tidy.res('# a comment', keep.comment = FALSE), character(0))
)