#' A Shiny app to format R code
#'
#' This function calls \code{\link{tidy.source}()} to format R code in a Shiny
#' app. The arguments of \code{tidy.source()} are presented in the app as input
#' widgets such as checkboxes.
#' @export
#' @examples if (interactive()) formatR::tidy.app()
tidy.app = function() {
  shiny::runApp(system.file('shiny', package = 'formatR'))
}
