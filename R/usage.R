##' Show the usage of a function.
##' Print the usage of a function in a formatted way.
##'
##' @param FUN the function
##' @param class provide the class to which the function is applied if
##' \code{FUN} is a S3 generic function
##' @param w the width of output
##' @return NULL
##' @author Yihui Xie <xie@@yihui.name>
##' @seealso \code{\link{tidy.source}}
##' @export
##' @examples
##' usage(var)
##'
##' usage(plot)
##' ## default method
##' usage(plot, "default")
##' ## on the 'lm' class
##' usage(plot, "lm")
##'
##' usage(usage)
##'
##' # narrower output
##' usage(barplot, "default", 0.75)
##'
usage = function(FUN, class = NULL, w = 0.77) {
    fn = as.character(substitute(FUN))
    if (!is.null(class)) {
        FUN = getS3method(fn, class)
    }
    x = paste(fn, substring(paste(capture.output(str(args(FUN))),
        collapse = ""), 9), sep = "")
    tidy.res = tidy.source(text = x, output = FALSE, keep.blank.line = FALSE,
        width.cutoff = w * getOption("width"))
    cat(tidy.res$text.tidy, "\n")
}
