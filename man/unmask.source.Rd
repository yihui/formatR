\name{unmask.source}
\alias{unmask.source}
\title{Restore the real source code from the masked text}
\usage{
unmask.source(text.mask, replace.tab = FALSE)
}
\arguments{
  \item{text.mask}{the masked source code}

  \item{replace.tab}{whether to replace \code{\\\\t} with
  \code{\\t}}
}
\value{
  the real source code (a character vector)
}
\description{
  Remove the masks from the code to restore the real code.
}
\examples{
library(formatR)

src = c("    # a single line of comments is preserved", 
    "1+1", "  ", "if(TRUE){", "x=1  # inline comments!", "}else{", 
    "x=2;print('Oh no... ask the right bracket to go away!')}", 
    "2+2+2    # 'short comments'", "lm(y~x1+x2)  ### only 'single quotes' are allowed in comments", 
    "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line")

x = tidy.source(text = src, output = FALSE)$text.mask

cat(x, sep = "\\n")

cat(unmask.source(x), sep = "\\n")
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\keyword{internal}
