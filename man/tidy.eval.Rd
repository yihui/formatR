\name{tidy.eval}
\alias{tidy.eval}
\title{Evaluate R code and mask the output by a prefix}
\usage{
tidy.eval(source = "clipboard", ..., file = "", prefix = "## ", envir = parent.frame())
}
\arguments{
  \item{source}{the input filename (by default the
  clipboard; see \code{\link{tidy.source}})}

  \item{...}{other arguments passed to
  \code{\link{tidy.source}}}

  \item{file}{the file to write by \code{\link[base]{cat}};
  by default the output is printed on screen}

  \item{prefix}{the prefix to mask the output}

  \item{envir}{the environment in which to evaluate the
  code (by default the parent environment; if we do not
  want to mess up with the parent environment, we can set
  \code{envir = NULL} or \code{envir = new.env()})}
}
\value{
  Evaluated R code with corresponding output (printed on
  screen or written in a file).
}
\description{
  This function is designed to insert the output of each
  chunk of R code into the source code without really
  breaking the source code, since the output is masked in
  comments.
}
\examples{
library(formatR)
## evaluate simple code as a character vector
tidy.eval(text = c("a<-1+1;a", "matrix(rnorm(10),5)"))

## evaluate a file
tidy.eval(source = file.path(system.file(package = "stats"), "demo", "nlm.R"), keep.blank.line = TRUE)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\references{
  \url{https://github.com/yihui/formatR/wiki/}
}
