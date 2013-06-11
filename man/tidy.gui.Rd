\name{tidy.gui}
\alias{tidy.gui}
\title{A GUI to format R code}
\usage{
tidy.gui(guiToolkit = "RGtk2")
}
\arguments{
  \item{guiToolkit}{the GUI toolkit to use}
}
\value{
  the text widget is returned
}
\description{
  Create a GUI (via GTK+ by default) to format R code.
}
\details{
  This function calls \code{\link{tidy.source}} to format R
  code. Spaces and indent will be added to the code
  automatically.

  We can either open an R source file or directly write R
  code in the text widget. Click the \samp{convert} button,
  and the code will become tidy. See
  \code{\link{tidy.source}} for more details.
}
\note{
  By default, the interface is based on GTK+ (R package
  \pkg{RGtk2}), but other options (\pkg{tcltk}, \pkg{rJava}
  and \bold{Qt}) are possible too.  See the examples below.
  Note the \samp{Font} button is only for the GTK+
  interface.
}
\examples{
\dontrun{
library("gWidgetsRGtk2")

## a GUI will show up on loading if one of the gWidgets toolkit is
## present (e.g. via library(gWidgetsRGtk2))
library(formatR)

g = tidy.gui()

## we have control over the text widget, e.g. set or get the text

svalue(g) = c("# a single line of comments is preserved", "1+1", 
    "if(TRUE){", paste("x=1 ", "# inline comments!"), "}else{", 
    "x=2;print('Oh no... ask the right bracket to go away!')}", 
    "1*3 # another inline comment")

## click 'Convert' now, and see

cat(svalue(g), sep = "\\n")  # get its value

## tcl/tk interface: need gWidgetstcltk package
tidy.gui("tcltk")

}
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\references{
  \url{https://github.com/yihui/formatR/wiki/}
  (screenshots)
}
\seealso{
  \code{\link{tidy.source}}
}
