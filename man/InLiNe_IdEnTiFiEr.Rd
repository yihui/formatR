\name{\%InLiNe_IdEnTiFiEr\%}
\alias{\%InLiNe_IdEnTiFiEr\%}
\title{A weird operator for internal use only}
\usage{
x \%InLiNe_IdEnTiFiEr\% y
}
\arguments{
  \item{x}{the argument before the operator}

  \item{y}{the argument after the operator}
}
\value{
  \code{x} (i.e. this operator always returns the object on
  the left-hand-side)
}
\description{
  This operator is almost meaningless; it is used to mask
  the inline comments.
}
\examples{
## we can use it with *anything*

1 \%InLiNe_IdEnTiFiEr\% 2

1:10 \%InLiNe_IdEnTiFiEr\% "asdf"

lm \%InLiNe_IdEnTiFiEr\% "garbage"
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link{tidy.source}}
}
\keyword{internal}
