#' Insert output to source code
#'
#' Evaluate R code by chunks, then insert the output to each chunk. As the
#' output is masked in comments, the source code will not break.
#' @param source The input file name (by default the clipboard; see
#'   \code{\link{tidy_source}()}).
#' @param ... Other arguments passed to \code{\link{tidy_source}()}.
#' @param file The file name to write to via \code{\link{cat}()}.
#' @param prefix The prefix to mask the output.
#' @param envir The environment in which to evaluate the code. By default the
#'   parent frame; set \code{envir = NULL} or \code{envir = new.env()} to avoid
#'   the possibility of contaminating the parent frame.
#' @return Evaluated R code with corresponding output (printed on screen or
#'   written to a file).
#' @export
#' @references \url{https://yihui.org/formatR/}
#' @examples library(formatR)
#' ## evaluate simple code as a character vector
#' tidy_eval(text = c('a<-1+1;a','matrix(rnorm(10),5)'))
#'
#' ## evaluate a file
#' tidy_eval(system.file('format', 'messy.R', package = 'formatR'))
tidy_eval = function(source = 'clipboard', ..., file = '', prefix = '## ', envir = parent.frame()) {
  txt = tidy_source(source, ..., output = FALSE)$text.tidy
  for (i in 1:length(txt)) {
    cat(txt[i], '\n', sep = '', file = file, append = TRUE)
    out = capture.output(eval(res <- parse_source(txt[i]), envir = envir))
    if (length(res) > 0L && length(out) > 0L) {
      cat(paste(prefix, out, sep = ''), sep = '\n', file = file, append = TRUE)
      cat('\n', file = file, append = TRUE)
    }
  }
}
