#' Deprecated functions
#'
#' These functions have been renamed and deprecated in \pkg{formatR}:
#' \code{tidy.source()} (use \code{\link{tidy_source}()}), \code{tidy.dir()}
#' (use \code{\link{tidy_dir}()}), and \code{tidy.eval()} (use
#' \code{\link{tidy_eval}()}).
#' @rdname deprecated
#' @keywords internal
#' @aliases formatR-deprecated
#' @param ... arguments passed from the old functions of the style
#'   \code{foo.bar()} to the new functions \code{foo_bar()}
#' @export
tidy.source = function(...) {
  .Deprecated('tidy_source', package = 'formatR')
  tidy_source(...)
}
#' @rdname deprecated
#' @export
tidy.dir = function(...) {
  .Deprecated('tidy_dir', package = 'formatR')
  tidy_dir(...)
}
#' @rdname deprecated
#' @export
tidy.eval = function(...) {
  .Deprecated('tidy_eval', package = 'formatR')
  tidy_eval(...)
}
