\name{tidy.app}
\alias{tidy.app}
\title{A Shiny app to format R code}
\usage{
tidy.app()
}
\description{
This function calls \code{\link{tidy.source}()} to format R code in a Shiny
app. The arguments of \code{tidy.source()} are presented in the app as input
widgets such as checkboxes.
}
\examples{
if (interactive()) formatR::tidy.app()
}
