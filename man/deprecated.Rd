% Please edit documentation in R/deprecated.R
\name{tidy.source}
\alias{formatR-deprecated}
\alias{tidy.dir}
\alias{tidy.eval}
\alias{tidy.source}
\title{Deprecated functions}
\usage{
tidy.source(...)

tidy.dir(...)

tidy.eval(...)
}
\arguments{
\item{...}{arguments passed from the old functions of the style
\code{foo.bar()} to the new functions \code{foo_bar()}}
}
\description{
These functions have been renamed and deprecated in \pkg{formatR}:
\code{tidy.source()} (use \code{\link{tidy_source}()}), \code{tidy.dir()}
(use \code{\link{tidy_dir}()}), and \code{tidy.eval()} (use
\code{\link{tidy_eval}()}).
}
\keyword{internal}
