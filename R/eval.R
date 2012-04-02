#' Evaluate R code and mask the output by a prefix
#' 
#' This function is designed to insert the output of each chunk of R code into 
#' the source code without really breaking the source code, since the output is 
#' masked in comments.
#' @param source the input filename (by default the clipboard; see 
#'   \code{\link{tidy.source}})
#' @param ... other arguments passed to \code{\link{tidy.source}}
#' @param file the file to write by \code{\link[base]{cat}}; by default the 
#'   output is printed on screen
#' @param prefix the prefix to mask the output
#' @param envir the environment in which to evaluate the code (by default the 
#'   global environment; if we do not want to mess up with the global 
#'   environment, we can set \code{envir = NULL} or \code{envir = new.env()})
#' @return Evaluated R code with corresponding output (printed on screen or 
#'   written in a file).
#' @author Yihui Xie <\url{http://yihui.name}>
#' @export
#' @references \url{https://github.com/yihui/formatR/wiki/}
#' @examples library(formatR)
#' ## evaluate simple code as a character vector
#' tidy.eval(text = c('a<-1+1;a','matrix(rnorm(10),5)'))
#' 
#' ## evaluate a file
#' tidy.eval(source = file.path(system.file(package = "stats"), "demo", "nlm.R"), keep.blank.line = TRUE)
tidy.eval = function(source = 'clipboard', ..., file = "", prefix = "## ", envir = globalenv()) {
  txt = tidy.source(source, ..., output = FALSE)$text.mask
  for(i in 1:length(txt)) {
    cat(unmask.source(txt[i]), '\n', file = file, append = TRUE)
    res = capture.output(eval(parse(text = txt[i]), envir = envir))
    if (length(res)) {
      cat(paste(prefix, res, sep = ''), sep = '\n', file = file, append = TRUE)
      cat('\n', file = file, append = TRUE)
    }
  }
}
