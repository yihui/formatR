% Please edit documentation in R/shiny.R
\name{tidy_app}
\alias{tidy_app}
\title{A Shiny app to format R code}
\usage{
tidy_app()
}
\description{
This function calls \code{\link{tidy_source}()} to format R code in a Shiny
app. The arguments of \code{tidy_source()} are presented in the app as input
widgets such as checkboxes.
}
\examples{
if (interactive()) formatR::tidy_app()
}
