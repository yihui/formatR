library(testit)

capture_usage = function(...) capture.output(usage(..., output = TRUE))
n_spaces = function(n) paste(character(n + 1L), collapse = ' ')

# Test usage() ------------------------------------------------------------

make_fn = function(arg) {
  eval(call('function', as.pairlist(arg), quote(expr = )))
}
subsets_lgl = function(n) {
  lapply(seq_len(2L ^ n) - 1L, function(.) as.logical(intToBits(.))[1L:n])
}
subsets = function(x) {
  lapply(subsets_lgl(length(x)), function(.) x[.])
}

args_pre  = alist(x1 = , x2 = , x3 = , x4 = , y1 = 1, y2 = 2, y3 = 3, y4 = 4)
args_post = alist(a = '', b = 'b')
args_wo_dots = subsets(args_pre)
args_w_dots  = do.call('c', lapply(subsets(args_post), function(.) {
  lapply(args_wo_dots, function(..) c(.., alist(... = ), .))
}))
args = c(args_wo_dots, args_w_dots)
fns = lapply(args, make_fn)

# test usage() for 7.5k+ functions
assert(
  'usage() output respects indent and line width, whenver this is feasible',
  {
    ops = options(formatR.indent = 4L)
    re = sprintf('^%s\\S', n_spaces(getOption('formatR.indent')))
    w0 = nchar('a_function()')
    # call with maximal set of arguments
    usg = paste(trimws(
      deparse(make_fn(c(args_pre, alist(... = ), args_post))),
      which = 'left'), collapse = '')
    w1 = nchar(sub('^function ', 'a_function', usg))

    assertions = lapply(fns, function(f) {
      a_function = f
      r = (w1 - w0) %/% 4L
      lapply(seq(w0, w1 + r, by = r), function(w) {
        out = capture_usage(a_function, w)
        c(
          'output was created'        = length(out) > 0L,
          'lines within width'        = nchar(out) <= w,
          'indentation by indent amt' = grepl(re, out[-1L])
        )
      })
    })
    options(ops)
    unlist(assertions)
  }
)

assert(
  'for an S3 method, usage() uses the generic function name in call signature',
  {
    out = capture_usage(barplot.default, 60L)
    TRUE
  },
  identical(out[1L], '## Default S3 method:'),
  identical(substr(out[2L], 1L, 8L), 'barplot(')
)

assert(
  'if width constraint is unfulfillable, usage() warns when fail is "warn"',
  {
    # Verify that width constraint is unfulfillable
    out = suppressWarnings(capture_usage(barplot.default, 30L, fail = 'warn'))
    any(nchar(out) > 30L)
  },
  has_warning(capture_usage(barplot.default, 30L, fail = 'warn'))
)

assert(
  'if width constraint is unfulfillable, usage() stops when fail is "stop"',
  {
    out = tryCatch(capture_usage(barplot.default, 30L, fail = 'stop'),
                   error = function(e) 'Error signaled')
    identical(out, 'Error signaled')
  }
)

assert(
  'if width constraint is unfulfillable, usage() is silent when fail is "none"',
  {
    out = tryCatch(capture_usage(barplot.default, 30L, fail = 'none'),
                   warning = function(w) 'Warning signaled',
                   error   = function(e) 'Error signaled')
    !out %in% c('Warning signaled', 'Error signaled')
  }
)

assert(
  'if width constraint is unfulfillable and fail is "warn" or "stop", then
   the lengths of all overflowing lines are shown',
  {
    out = capture.output(
      suppressWarnings(usage(barplot.default, 30L, fail = 'warn'))
    )
    warn = capture.output(cat(
      tryCatch(usage(barplot.default, 30L, fail = 'warn'),
               warning = conditionMessage)
    ))[-1L]
    bad_lines = out[nchar(out) > 30L]
    overflow_out  = nchar(bad_lines)
    overflow_warn = as.integer(sub('^\\(([[:digit:]]*)\\).*', '\\1', warn))
    identical(overflow_out, overflow_warn)
  }
)

assert(
  'usage() fits entire call on one line if it falls within width',
  {
    foo = function(bar, ..., baz = "baz") {}
    width = nchar('foo(bar, ..., baz = "baz")')
    vapply(seq(width, width + 60L, by = 5L), function(w) {
      out = usage(foo, width = w, output = FALSE)
      identical(nchar(out), width)
    }, logical(1))
  }
)

assert(
  'usage() breaks lines maximally and uniformly when all lines of same length',
  {
    foo = function(bar, baz = 0,
                   buzz, x, ...,
                   y = 2, z = 3) {}
    w = nchar('foo(bar, baz = 0,')
    out = capture_usage(foo, width = w, indent.by.FUN = TRUE)
    TRUE
  },
  identical(length(out), 3L),
  all(nchar(out) == w)
)

assert(
  'usage() indents by getOption("formatR.indent", 4L),
   when indent.by.FUN is FALSE',
  {
    foo = function(bar, ..., baz = "baz") {}
    TRUE
  },
  {
    ops = options(formatR.indent = NULL)
    out = capture_usage(foo, width = 20L, indent.by.FUN = FALSE)
    options(ops)
    identical(out[2L], '    baz = "baz")')
  },
  {
    ops = options(formatR.indent = 2L)
    out = capture_usage(foo, width = 20L)
    options(ops)
    identical(out[2L], '  baz = "baz")')
  }
)

assert(
  'usage() indents by function name width, when indent.by.FUN is TRUE',
  {
    re = function(n) sprintf('^%s\\S', n_spaces(n))
    out1 = capture_usage(barplot.default, width = 60L, indent.by.FUN = TRUE)
    out2 = capture_usage(stats::lm, width = 60, indent.by.FUN = TRUE)
    TRUE
  },
  grepl(re(nchar('barplot(')), out1[-(1L:2L)]),
  grepl(re(nchar('lm(')), out2[-1L])
)

assert(
  'usage() breaks line on function name, if function name exceeds width',
  {
    reallylongfunctionname = function() {}
    w = nchar('reallylongfunctionname')
    out = capture_usage(reallylongfunctionname, w, fail = 'none')
    identical(out, 'reallylongfunctionname()')
  },
  {
    warn = tryCatch(usage(reallylongfunctionname, w, fail = 'warn'),
                    warning = function(w) {
                      capture.output(cat(conditionMessage(w)))
                    })
    l = nchar('reallylongfunctionname()')
    identical(warn[2L], sprintf('(%s) \"reallylongfunctionname()\"', l))
  },
  {
    reallylongfunctionname = function(bar, baz, ..., a, b, c, d, e) {}
    res = lapply(c(5L, 10L, 20L), function(w) {
      list(
        out  = capture_usage(reallylongfunctionname, w, fail = 'none'),
        warn = tryCatch(usage(reallylongfunctionname, w, fail = 'warn'),
                        warning = function(w) {
                          capture.output(cat(conditionMessage(w)))
                        })
      )
    })
    l = nchar('reallylongfunctionname(')
    vapply(res, function(.) {
      all(
        nchar(.$out[-1L]) <= w,
        identical(.$warn[2L], sprintf('(%s) \"reallylongfunctionname(\"', l))
      )
    }, logical(1))
  }
)

# Test internal functions (optional) --------------------------------------

exprs = list(
  quote(foo()),
  quote(foo(...)),
  quote(foo(bar)),
  quote(foo(bar, baz)),
  quote(foo(bar = 1, baz)),
  quote(foo(bar = 1, baz = "")),
  quote(foo(bar, baz = 2)),
  quote(foo(bar, ..., baz = 2)),
  quote(foo(bar, ..., baz))
)
counts = list(
  c(5L),
  c(4L, 4L),
  c(4L, 4L),
  c(4L, 4L, 5L),
  c(4L, 8L, 5L),
  c(4L, 8L, 10L),
  c(4L, 4L, 9L),
  c(4L, 4L, 5L, 9L),
  c(4L, 4L, 5L, 5L)
)
totals_manual = lapply(counts, cumsum)
totals_count_tokens = lapply(exprs, count_tokens)
assert(
  'count_tokens() matches manual count of tokens',
  unlist(
    Map(function(x, y) isTRUE(all.equal(x, y, check.names = FALSE)),
        totals_count_tokens, totals_manual)
  )
)
