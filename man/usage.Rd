\name{usage}
\alias{usage}
\title{Show the usage of a function}
\usage{
usage(FUN, class = NULL, w = 0.77)
}
\arguments{
  \item{FUN}{the function}

  \item{class}{provide the class to which the function is
  applied if \code{FUN} is a S3 generic function}

  \item{w}{the width of output}
}
\value{
  NULL
}
\description{
  Print the usage of a function in a formatted way.
}
\examples{
usage(var)

usage(plot)
## default method
usage(plot, "default")
## on the 'lm' class
usage(plot, "lm")

usage(usage)

# narrower output
usage(barplot, "default", 0.75)
}
\author{
  Yihui Xie <xie@yihui.name>
}
\seealso{
  \code{\link{tidy.source}}
}
