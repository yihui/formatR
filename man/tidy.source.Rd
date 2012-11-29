\name{tidy.source}
\alias{tidy.source}
\title{Reformat R code while preserving blank lines and comments}
\usage{
tidy.source(source = "clipboard", keep.comment = getOption("keep.comment", TRUE), 
    keep.blank.line = getOption("keep.blank.line", TRUE), replace.assign = getOption("replace.assign", 
        FALSE), left.brace.newline = getOption("left.brace.newline", FALSE), reindent.spaces = getOption("reindent.spaces", 
        4), output = TRUE, text = NULL, width.cutoff = getOption("width"), ...)
}
\arguments{
  \item{source}{a character string: location of the source
  code (default to be the clipboard; this means we can copy
  the code to clipboard and use \code{tidy.souce()} without
  specifying the argument \code{source})}

  \item{keep.comment}{whether to keep comments (\code{TRUE}
  by default)}

  \item{keep.blank.line}{whether to keep blank lines
  (\code{TRUE} by default)}

  \item{replace.assign}{whether to replace the assign
  operator \code{=} with \code{<-}}

  \item{left.brace.newline}{whether to put the left brace
  \code{\{} to a new line (default \code{FALSE})}

  \item{reindent.spaces}{number of spaces to indent the
  code (default 4)}

  \item{output}{output to the console or a file using
  \code{\link[base]{cat}}?}

  \item{text}{an alternative way to specify the input: if
  it is \code{NULL}, the function will read the source code
  from the \code{source} argument; alternatively, if
  \code{text} is a character vector containing the source
  code, it will be used as the input and the \code{source}
  argument will be ignored}

  \item{width.cutoff}{passed to
  \code{\link[base]{deparse}}: integer in [20, 500]
  determining the cutoff at which line-breaking is tried
  (default to be \code{getOption("width")})}

  \item{...}{other arguments passed to
  \code{\link[base]{cat}}, e.g.  \code{file} (this can be
  useful for batch-processing R scripts, e.g.
  \code{tidy.source(source = 'input.R', file =
  'output.R')})}
}
\value{
  A list with components \item{text.tidy}{the reformatted
  code as a character vector} \item{text.mask}{the code
  containing comments, which are masked in assignments or
  with the weird operator}
}
\description{
  This function returns reformatted source code; it tries
  to preserve blank lines and comments, which is different
  with \code{\link[base]{parse}}. It can also replace
  \code{=} with \code{<-} where \code{=} means assignments,
  and reindent code by a specified number of spaces
  (default is 4).
}
\details{
  This function helps the users to tidy up their source
  code in a sense that necessary indents and spaces will be
  added, but comments will be preserved if
  \code{keep.comment = TRUE}. See the references to know
  how this function actually works.
}
\note{
  Be sure to read the reference to know other limitations.
}
\examples{
library(formatR)

## use the 'text' argument
src = c("    # a single line of comments is preserved", "1+1", "  ", "if(TRUE){", 
    "x=1  # inline comments", "}else{", "x=2;print('Oh no... ask the right bracket to go away!')}", 
    "1*3 # one space before this comment will become two!", "2+2+2    # 'short comments'", 
    "   ", "lm(y~x1+x2, data=data.frame(y=rnorm(100),x1=rnorm(100),x2=rnorm(100)))  ### only 'single quotes' are allowed in comments", 
    "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line", 
    "'a character string with \\t in it'", "", paste("## here is a", paste(rep("long", 
        20), collapse = " "), "comment"))

## source code
cat(src, sep = "\\n")

## the formatted version
tidy.source(text = src)

## preserve blank lines
tidy.source(text = src, keep.blank.line = TRUE)

## indent with 2 spaces
tidy.source(text = src, reindent.spaces = 2)

## discard comments!
tidy.source(text = src, keep.comment = FALSE)

## wanna see the gory truth??
tidy.source(text = src, output = FALSE)$text.mask


## tidy up the source code of image demo
x = file.path(system.file(package = "graphics"), "demo", "image.R")

# to console
tidy.source(x)

# to a file
f = tempfile()
tidy.source(x, keep.blank.line = TRUE, file = f)

## check the original code here and see the difference
file.show(x)
file.show(f)

## use global options
options(keep.comment = TRUE, keep.blank.line = FALSE)
tidy.source(x)

## if you've copied R code into the clipboard
if (interactive()) {
    tidy.source("clipboard")
    ## write into clipboard again
    tidy.source("clipboard", file = "clipboard")
}

## the if-else structure
tidy.source(text = c("{if(TRUE)1 else 2; if(FALSE){1+1", "## comments", "} else 2}"))
}
\author{
  Yihui Xie <\url{http://yihui.name}> with substantial
  contribution from Yixuan Qiu
  <\url{http://yixuan.cos.name}>
}
\references{
  \url{https://github.com/yihui/formatR/wiki/} (an
  introduction to this package, with examples and further
  notes)

  The package vignette also contains some examples (see
  \code{vignette('formatR', package = 'formatR')}.

  Hadley's style guide:
  \url{https://github.com/hadley/devtools/wiki/Style}
}
\seealso{
  \code{\link[base]{parse}}, \code{\link[base]{deparse}},
  \code{\link[base]{cat}}
}
\keyword{IO}
