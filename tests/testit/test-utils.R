library(testit)

assert('move_leftbrace() works', {
  # no indent before abc, so no indent before {
  (move_leftbrace(c('abc() {', '    }')) %==% c('abc()', '{', '    }'))
  # 3 spaces before abc, 3 before {
  (move_leftbrace(c('   a() {', '}')) %==% c('   a()', '   {', '}'))
  (move_leftbrace(rep(c('   a() {', '}'), 5)) %==% rep(c('   a()', '   {', '}'), 5))
  # blank lines are not removed
  (move_leftbrace(c('a', '', 'b')) %==% c('a', '', 'b'))
  (move_leftbrace(c('if (TRUE) {', '  if (FALSE) {', '    1', '  }', '}')) %==%
      c('if (TRUE)', '{', '  if (FALSE)', '  {', '    1', '  }', '}'))
  (move_leftbrace(c('if (TRUE) {', '  1', '} else {', '  2}')) %==%
      c('if (TRUE)', '{', '  1', '} else', '{', '  2}'))
})

assert('reindent_lines() works', {
  (reindent_lines('') %==% '')
  (reindent_lines(c('', '')) %==% c('', ''))
  (reindent_lines('    ', n = 2) %==% '  ')
  (reindent_lines(c('if (TRUE) {', '    1', '}'), n = 2) %==% c('if (TRUE) {', '  1', '}'))
})
