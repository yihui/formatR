#' Show the usage of a function
#'
#' Print the reformatted usage of a function. The arguments of the function are
#' searched by \code{\link{argsAnywhere}}, so the function can be either
#' exported or non-exported in a package. S3 methods will be marked.
#' @param FUN the function name
#' @param width the width of output (passed to \code{width.cutoff} in
#'   \code{\link{tidy_source}})
#' @param tidy whether to reformat the usage code
#' @param output whether to write the output to the console (via
#'   \code{\link{cat}})
#' @param indent.by.FUN \code{TRUE} or \code{FALSE}: Should subsequent lines be
#'   indented by the width of the function name?
#' @return The R code for the usage is returned as a character string
#'   (invisibly).
#' @seealso \code{\link{tidy_source}}
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
#' usage(barplot.default, width = 60)  # narrower output
usage = function(FUN, width = getOption('width'), tidy = TRUE, output = TRUE,
                 indent.by.FUN = FALSE) {
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

  indent <- if (indent.by.FUN) {
    # indent by function name plus "("
    nchar(if (isS3) gen else fn) + 1L
  } else {
    # default indent for tidy_source()
    getOption('formatR.indent', 4L)
  }
  tidy.res = tidy_source(text = res, output = FALSE, width.cutoff = width,
                         indent = indent)
  if (output) cat(tidy.res$text.tidy, sep = '\n')
  invisible(tidy.res$text.tidy)
}

# Alternative implementation of usage() -----------------------------------

count_tokens = function(.call) {
  # +1 for value-delimiting "(", ",", or ")"
  nc_val = nchar(vapply(.call, deparse, character(1))) + 1L
  # nchar() of argument names
  nc_nm = nchar(names(.call[-1L]))
  # +3 for ' = '
  nc_nm[nc_nm != 0L] = nc_nm[nc_nm != 0L] + 3L
  # +1 for space before name, beyond the first argument
  nc_nm[-1L] = nc_nm[-1L] + 1L
  # Function itself is not a named component
  nc_nm = c(0L, nc_nm)

  cumsum(nc_val + nc_nm)
}

find_breaks = function(counts, width, indent, counted = 0L) {
  if (!length(counts)) {
    return(integer())
  }
  buffer = if (counted == 0L) 0L else indent
  i = which.min(counts - counted + buffer <= width) - 1L
  if (i == 0L) i = length(counts)

  c(counts[i], Recall(counts[-(1L:i)], width, indent, counts[i]))
}

tidy_usage = function(nm, usg, width, indent) {
  text = paste(trimws(usg, which = 'both'), collapse = ' ')
  text = sub(sprintf('^%s\\s*', nm), nm, text)
  expr = parse(text = text)[[1L]]
  breaks = c(0L, find_breaks(count_tokens(expr), width, indent))
  newline = paste(c('\n', character(indent)), collapse = ' ')

  paste(
    vapply(1L:(length(breaks) - 1L), function(i) {
      substr(text, breaks[i] + 1L, breaks[i + 1L])
    }, character(1L)),
    collapse = newline
  )
}

#' @rdname usage
#' @export
usage2 = function(FUN, width = getOption('width'), tidy = TRUE, output = TRUE,
                  indent.by.FUN = FALSE) {
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

  # --- Function identical to usage() up to this point ---

  nm  = if (isS3) gen else fn
  usg = if (isS3) res[-1L] else res
  indent = if (indent.by.FUN) {
    nchar(nm)
  } else {
    # -1 from default indent for tidy_source() compensated by width of "("
    getOption('formatR.indent', 4L) - 1L
  }
  out = tidy_usage(nm, usg, width, indent)
  if (isS3) out = c(res[1L], out)

  if (output) cat(out, sep = '\n')
  invisible(out)
}
