##' Show the usage of a function
##'
##' Print the reformatted usage of a function. The arguments of the
##' function are searched by \code{\link[utils]{argsAnywhere}}, so the
##' function can be either exported or non-exported in a package. S3
##' methods will be marked.
##' @param FUN the function name
##' @param width the width of output
##' @return \code{NULL}; the usage is printed on screen
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{tidy.source}}
##' @export
##' @examples library(formatR)
##' usage(var)
##'
##' usage(plot)
##'
##' usage(plot.default)  # default method
##' usage(plot.lm)  # on the 'lm' class
##'
##' usage(usage)
##'
##' usage(barplot.default, width = 0.6)  # narrower output
usage = function(FUN, width = 0.77) {
    fn = as.character(substitute(FUN))
    res = capture.output(do.call(argsAnywhere, list(fn)))
    if (identical(res, 'NULL')) return()
    res[1] = substring(res[1], 9)  # rm 'function ' in the beginning
    if (grepl('.', fn, fixed = TRUE)) {
        n = length(parts <- strsplit(fn, '.', fixed = TRUE)[[1]])
        for (i in 2:n) {
            gen = paste(parts[1L:(i - 1)], collapse = ".")
            cl = paste(parts[i:n], collapse = ".")
            if (gen == "" || cl == "")
                next
            if (!is.null(f <- getS3method(gen, cl, TRUE)) && !is.null(environment(f))) {
                res[1] = paste(gen, res[1])
                header = if (cl == 'default')
                    '## Default S3 method:' else sprintf("## S3 method for class '%s'", cl)
                res = c(header, res)
            }
        }
    } else res[1] = paste(fn, res[1])
    if ((n <- length(res)) > 1 && res[n] == 'NULL') res = res[-n]  # rm last element 'NULL'
    tidy.res =
        tidy.source(text = res, output = FALSE, width.cutoff = width * getOption("width"))
    cat(tidy.res$text.tidy, sep = '\n')
}
