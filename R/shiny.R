#' A Shiny app to format R code
#'
#' Run a Shiny app that formats R code via \code{\link{tidy_source}()}. This app
#' uses input widgets, such as checkboxes, to pass arguments to
#' \code{tidy_source()}.
#' @export
#' @examples if (interactive()) formatR::tidy_app()
tidy_app = function() {
  shiny::runApp(system.file('shiny', package = 'formatR'))
}
