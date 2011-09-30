##' Show the usage of a function
##'
##' Print the usage of a function in a formatted way.
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
usage = function(FUN, class = NULL, w = 0.77) {
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
