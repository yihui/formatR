# the code below was mostly contributed by @egnha from
# https://github.com/yihui/formatR/pull/66

deparse_collapse = function(x) {
  d = deparse(x)
  if (length(d) > 1L) {
    paste(trimws(d, which = 'both'), collapse = ' ')
  } else {
    d
  }
}

count_tokens = function(.call) {
  if (length(.call) == 1L) {
    # +2 for '()'
    return(nchar(.call) + 2L)
  }
  # +1 for value-delimiting '(', ',', or ')'
  cnt_val = nchar(vapply(.call, deparse_collapse, character(1L))) + 1L
  nms = names(.call[-1L])
  if (is.null(nms)) nms = character(length(.call[-1L]))
  # nchar() of argument names
  cnt_nm = nchar(nms)
  # +3 for ' = ', for argument-value pairs
  cnt_nm[cnt_nm != 0L] = cnt_nm[cnt_nm != 0L] + 3L
  # +1 for space before name, beyond the first argument
  cnt_nm[-1L] = cnt_nm[-1L] + 1L
  # function itself is not a named component
  cnt_nm = c(0L, cnt_nm)
  cumsum(cnt_val + cnt_nm)
}

# counts is a strictly increasing, positive integer vector
find_breaks = function(counts, width, indent, track, counted = 0L) {
  if (!length(counts)) {
    return(list(breaks = NULL, overflow = NULL))
  }
  overflow = NULL
  shift = if (counted == 0L) 0L else indent
  fits = counts - counted + shift <= width
  i = which.min(fits) - 1L
  if (i == 0L) {
    if (fits[[1L]]) {
      # all components of fits_on_line are TRUE
      i = length(counts)
    } else {
      # all components of fits_on_line are FALSE
      overflow = track(counted, counts[1L], shift)
      i = 1L
    }
  }
  post_space = if (i == 1L && counted == 0L) 0L else 1L
  rest = Recall(counts[-(1L:i)], width, indent, track, counts[i] + post_space)
  list(
    breaks   = c(counts[i], rest$breaks),
    overflow = c(overflow, rest$overflow)
  )
}

overflow_message = function(overflow, width, indent, text) {
  header = sprintf('Could not fit all lines to width %s (with indent %s):',
                   width, indent)
  idxs = seq_along(overflow)
  args = vapply(idxs[idxs %% 3L == 1L], function(i) {
    l = paste(c(rep(' ', overflow[i + 2L]),
                trimws(substr(text, overflow[i] + 1L, overflow[i + 1L]),
                       which = 'left')),
              collapse = '')
    sprintf('(%s) \"%s\"', nchar(l), l)
  }, character(1L))
  one_string(c(header, args))
}

tidy_usage = function(nm, usg, width, indent, fail) {
  text = paste(trimws(usg, which = 'both'), collapse = ' ')
  text = sub(sprintf('^%s\\s*', nm), nm, text)
  expr = parse(text = text)[[1L]]
  track_overflow = if (fail == 'none') function(...) NULL else base::c
  breaks = find_breaks(count_tokens(expr), width, indent, track_overflow)
  if (length(breaks$overflow)) {
    signal = switch(fail, stop = 'stop', warn = 'warning')
    msg = overflow_message(breaks$overflow, width, indent, text)
    getFromNamespace(signal, 'base')(msg, call. = FALSE)
  }
  breaks = c(0L, breaks$breaks)
  newline = paste(c('\n', character(indent)), collapse = ' ')
  paste(
    vapply(1L:(length(breaks) - 1L), function(i) {
      trimws(substr(text, breaks[i] + 1L, breaks[i + 1L]), which = 'left')
    }, character(1L)),
    collapse = newline
  )
}

#' Show the usage of a function
#'
#' Print the reformatted usage of a function. The arguments of the function are
#' searched by \code{\link{argsAnywhere}()}, so the function can be either
#' exported or non-exported from a package. S3 methods will be marked.
#' @param FUN The function name.
#' @param width The width of the output.
#' @param tidy Whether to reformat the usage code.
#' @param output Whether to print the output to the console (via
#'   \code{\link{cat}()}).
#' @param indent.by.FUN Whether to indent subsequent lines by the width of the
#'   function name (see \dQuote{Details}).
#' @param fail A character string that represents the action taken when the
#'   width constraint is unfulfillable. "warn" and "stop" will signal warnings
#'   and errors, while "none" will do nothing.
#' @return Reformatted usage code of a function, in character strings
#'   (invisible).
#' @details Line breaks in the output occur between arguments. In particular,
#'   default values of arguments will not be split across lines.
#'
#'   When \code{indent.by.FUN} is \code{FALSE}, indentation is set by the option
#'   \code{\link{getOption}("formatR.indent", 4L)}, the default value of the
#'   \code{indent} argument of \code{\link{tidy_source}()}.
#' @seealso \code{\link{tidy_source}()}
#' @export
#' @examples library(formatR)
#' usage(var)
#'
#' usage(plot)
#'
#' usage(plot.default)  # default method
#' usage('plot.lm')  # on the 'lm' class
#'
#' usage(usage)
#'
#' usage(barplot.default, width = 60)  # output lines have 60 characters or less
#'
#' # indent by width of 'barplot('
#' usage(barplot.default, width = 60, indent.by.FUN = TRUE)
#'
#' \dontrun{
#' # a warning is raised because the width constraint is unfulfillable
#' usage(barplot.default, width = 30)
#' }
usage = function(FUN, width = getOption('width'), tidy = TRUE, output = TRUE,
                 indent.by.FUN = FALSE, fail = c('warn', 'stop', 'none')) {
  fail = match.arg(fail)
  fn = as.character(substitute(FUN))
  res = capture.output(if (is.function(FUN)) args(FUN) else {
    do.call(argsAnywhere, list(fn))
  })
  if (identical(res, 'NULL')) return()
  res[1] = substring(res[1], 9)  # rm 'function ' in the beginning
  isS3 = FALSE
  if (length(fn) == 3 && (fn[1] %in% c('::', ':::'))) fn = fn[3]
  if (grepl('.', fn, fixed = TRUE)) {
    n = length(parts <- strsplit(fn, '.', fixed = TRUE)[[1]])
    for (i in 2:n) {
      gen = paste(parts[1L:(i - 1)], collapse = ".")
      cl = paste(parts[i:n], collapse = ".")
      if (gen == "" || cl == "") next
      if (!is.null(f <- getS3method(gen, cl, TRUE)) && !is.null(environment(f))) {
        res[1] = paste(gen, res[1])
        header = if (cl == 'default')
          '## Default S3 method:' else sprintf("## S3 method for class '%s'", cl)
        res = c(header, res)
        isS3 = TRUE
        break
      }
    }
  }
  if (!isS3) res[1] = paste(fn, res[1])
  if ((n <- length(res)) > 1 && res[n] == 'NULL') res = res[-n]  # rm last element 'NULL'
  if (!tidy) {
    if (output) cat(res, sep = '\n')
    return(invisible(res))
  }

  nm  = if (isS3) gen else fn
  usg = if (isS3) res[-1L] else res
  indent = if (indent.by.FUN) {
    # +1 for '('
    nchar(nm) + 1L
  } else {
    # Default indent for tidy_source()
    getOption('formatR.indent', 4L)
  }
  out = tidy_usage(nm, usg, width, indent, fail)
  if (isS3) out = c(res[1L], out)

  if (output) cat(out, sep = '\n')
  invisible(out)
}
