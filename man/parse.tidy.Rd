\name{parse.tidy}
\alias{deparse.tidy}
\alias{parse.tidy}
\title{Modified versions of parse() and deparse()}
\usage{
parse.tidy(text, ...)

deparse.tidy(expr, ...)
}
\arguments{
  \item{text}{the source code as a character string to be
  passed to \code{\link{tidy.source}}}

  \item{...}{for \code{\link{parse.tidy}}: other arguments
  to be passed to \code{\link{tidy.source}}; for
  \code{\link{deparse.tidy}}: arguments to be passed to
  \code{\link[base]{deparse}}}

  \item{expr}{the unevaluated expressions (ideally as
  results from \code{\link{parse.tidy}})}
}
\value{
  \code{\link{parse.tidy}} returns the unevaluated
  expressions; \code{\link{deparse.tidy}} returns the
  character strings
}
\description{
  These two functions parse and deparse the masked source
  code.
}
\details{
  For \code{\link{parse.tidy}}, the source code is masked
  to preserve comments, then this function uses
  \code{\link[base]{parse}} to return the parsed but
  unevaluated expressions in a list.

  For \code{\link{deparse.tidy}}, it uses
  \code{\link[base]{deparse}} to turn the unevaluated (and
  masked) expressions into character strings; the masks
  will be removed to restore the real source code. See
  \code{\link{unmask.source}}.
}
\note{
  These functions are mainly designed for the package
  \pkg{pgfSweave}; they may not be useful to general users.
}
\examples{
src = c("    # a single line of comments is preserved", "1+1", "  ", "if(TRUE){", 
    "x=1  # comments begin with at least 2 spaces!", "}else{", "x=2;print('Oh no... ask the right bracket to go away!')}", 
    "1*3 # this comment will be dropped!", "2+2+2    # 'short comments'", "lm(y~x1+x2)  ### only 'single quotes' are allowed in comments", 
    "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line")

(expr = parse.tidy(src))

parse.tidy(src, keep.blank.line = TRUE)

cat(deparse.tidy(expr))

deparse.tidy(expr, width.cutoff = 50)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link[base]{parse}}, \code{\link[base]{deparse}},
  \code{\link{tidy.source}}
}
\keyword{internal}
