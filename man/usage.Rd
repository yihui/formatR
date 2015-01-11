% Please edit documentation in R/usage.R
\name{usage}
\alias{usage}
\title{Show the usage of a function}
\usage{
usage(FUN, width = getOption("width"), tidy = TRUE, output = TRUE)
}
\arguments{
\item{FUN}{the function name}

\item{width}{the width of output (passed to \code{width.cutoff} in
\code{\link{tidy_source}})}

\item{tidy}{whether to reformat the usage code}

\item{output}{whether to write the output to the console (via
\code{\link{cat}})}
}
\value{
The R code for the usage is returned as a character string
  (invisibly).
}
\description{
Print the reformatted usage of a function. The arguments of the function are
searched by \code{\link{argsAnywhere}}, so the function can be either
exported or non-exported in a package. S3 methods will be marked.
}
\examples{
library(formatR)
usage(var)

usage(plot)

usage(plot.default)  # default method
usage(plot.lm)  # on the 'lm' class

usage(usage)

usage(barplot.default, width = 60)  # narrower output
}
\seealso{
\code{\link{tidy_source}}
}
