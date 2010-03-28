\name{formatR}
\alias{formatR}
\title{
GUI to format R code
}
\description{
Create a GUI to format R code.
}
\usage{
formatR()
}
\details{
This function calls \code{\link[animation]{tidy.source}} to format R code. Spaces and indent will be added to the code automatically.

We can either open an R source file or directly write R code in the text widget. Click the ``convert'' button, and the code will become tidy. See \code{\link[animation]{tidy.source}} for more details.
}
\value{
Invisible \code{NULL}.
}
\note{
Inline comments will be removed, as documented in \code{\link[animation]{tidy.source}}.

For the time being, multi-byte characters cannot be handled correctly.
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
\seealso{
\code{\link[animation]{tidy.source}}
}
\examples{
\dontrun{

formatR()

}
}
