\name{tidy_dir}
\alias{tidy_dir}
\title{Format the R scripts under a directory}
\usage{
tidy_dir(path = ".", recursive = FALSE, ...)
}
\arguments{
\item{path}{the directory}

\item{recursive}{whether to recursively look for R scripts under \code{path}}

\item{...}{other arguments to be passed to \code{\link{tidy_source}}}
}
\value{
Invisible \code{NULL}.
}
\description{
This function first looks for all the R scripts under a directory (using the
pattern \code{"[.][RrSsQq]$"}), then uses \code{\link{tidy_source}} to tidy
these scripts. The original scripts will be overwritten with reformatted code
if reformatting was successful. You may need to back up the original
directory first if you do not fully understand the tricks
\code{\link{tidy_source}} is using.
}
\examples{
library(formatR)

path = tempdir()
file.copy(system.file("demo", package = "base"), path, recursive = TRUE)
tidy_dir(path, recursive = TRUE)
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
\seealso{
\code{\link{tidy_source}}
}
