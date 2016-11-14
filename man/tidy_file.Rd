% Please edit documentation in R/tidy.R
\name{tidy_file}
\alias{tidy_file}
\title{Format the R scripts of a specific file or vector of files}
\usage{
tidy_file(fvec, ...)
}
\arguments{
\item{fvec}{the file, or vector of files}

\item{...}{additional params of tidy_source}
}
\value{
Invisible \code{NULL}.
}
\description{
The original script will be overwritten with reformatted code
if reformatting was successful. You may need to back up the original
first if you do not fully understand the tricks
\code{\link{tidy_source}} is using.
}
\details{
A wrapper of tidy_source to overwrite a file with tidied version
}
\author{
edlee123
}
