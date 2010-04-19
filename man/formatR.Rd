\name{formatR}
\alias{formatR}
\title{
GUI to format R code
}
\description{
Create a GUI (via GTK+ by default) to format R code.
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
\references{\url{http://yihui.name/en/2010/04/formatr-farewell-to-ugly-r-code/}}
\note{
Inline comments will be removed, as documented in \code{\link[animation]{tidy.source}}.

For the time being, multi-byte characters cannot be handled correctly.

By default, the interface is based on GTK+ (R package \bold{RGtk2}), but other options (\bold{tcltk} and \bold{rJava}) are possible too. See the examples below. Note the ``Font'' button is only for the GTK+ interface and it can only set font styles for the selected texts.
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
\seealso{
\code{\link[animation]{tidy.source}}
}
\examples{
\dontrun{

## a GUI will show up on loading
library(formatR)

## manually call the GUI
formatR()

## tcl/tk interface: need gWidgetstcltk package
options(guiToolkit = "tcltk")
formatR()

## Java interface: need gWidgetsrJava package
options(guiToolkit = "rJava")
formatR()

}
}
