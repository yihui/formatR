#' Show the usage of a function
#'
#' Print the reformatted usage of a function. The arguments of the function are
#' searched by \code{\link{argsAnywhere}}, so the function can be either
#' exported or non-exported in a package. S3 methods will be marked.
#' @param FUN the function name
#' @param width the width of output (passed to \code{width.cutoff} in
#'   \code{\link{tidy_source}})
#' @param tidy whether or not to reformat the usage code
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
#' usage(plot.lm)  # on the 'lm' class
#'
#' usage(usage)
#'
#' usage(barplot.default, width = 60)  # narrower output
usage = function(FUN, width = getOption('width'), tidy = TRUE) {
  fn = as.character(substitute(FUN))
  res = capture.output(do.call(argsAnywhere, list(fn)))
  if (identical(res, 'NULL')) return()
  res[1] = substring(res[1], 9)  # rm 'function ' in the beginning
  isS3 = FALSE
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
    cat(res, sep = '\n')
    return(invisible(res))
  }

  if (width <= 1) {
    warning("'width' should no longer be specified as a proportion")
    width = width * getOption("width")
  }
  tidy.res = tidy_source(text = res, output = FALSE, width.cutoff = width)
  cat(tidy.res$text.tidy, sep = '\n')
  invisible(tidy.res$text.tidy)
}
