library(formatR)

with(getNamespace('formatR'), {
  stopifnot(
    # no indent before abc, so no indent before {
    identical(move_leftbrace('abc() {\n    }'), 'abc()\n{\n    }'),
    # 3 spaces before abc, 3 before {
    identical(move_leftbrace('   a() {\n}'), c('   a()\n   {\n}')),
    # blank lines are not removed
    identical(move_leftbrace(c('a', '', 'b')), c('a', '', 'b')),
    identical(move_leftbrace('if (TRUE) {\n  if (FALSE) {\n    1\n  }\n}'),
              'if (TRUE)\n{\n  if (FALSE)\n  {\n    1\n  }\n}'),
    identical(move_leftbrace('if (TRUE) {\n  1\n} else {\n  2}'),
              'if (TRUE)\n{\n  1\n} else\n{\n  2}')
  )

  stopifnot(
    identical(reindent_lines(''), ''),
    identical(reindent_lines(c('', '')), c('', '')),
    identical(reindent_lines('    ', 2), '  '),
    identical(reindent_lines('if (TRUE) {\n    1\n}', 2), 'if (TRUE) {\n  1\n}')
  )
})
