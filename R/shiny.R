#' A Shiny app to format R code
#'
#' This function calls \code{\link{tidy_source}()} to format R code in a Shiny
#' app. The arguments of \code{tidy_source()} are presented in the app as input
#' widgets such as checkboxes.
#' @export
#' @examples if (interactive()) formatR::tidy_app()
tidy_app = function() {
  shiny::runApp(system.file('shiny', package = 'formatR'))
}
