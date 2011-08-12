\name{tidy.source}
\alias{tidy.source}
\title{`Tidy up' R code while preserving comments}
\usage{
tidy.source(source = "clipboard", keep.comment, keep.blank.line, 
    keep.space, replace.assign, output = TRUE, text = NULL, width.cutoff = 0.75 * 
        getOption("width"), ...)
}
\arguments{
  \item{source}{a string: location of the source code
  (default to be the clipboard; this means we can copy the
  code to clipboard and use \code{tidy.souce()} without
  specifying the argument \code{source})}

  \item{keep.comment}{logical value: whether to keep
  comments or not? (\code{TRUE} by default)}

  \item{keep.blank.line}{logical value: whether to keep
  blank lines or not? (\code{FALSE} by default)}

  \item{keep.space}{logical: whether to preserve the
  leading spaces in the single lines of comments (default
  \code{FALSE})}

  \item{replace.assign}{logical: whether to replace the
  assign operator \code{=} with \code{<-}}

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
  (default to be \code{0.75 * getOption("width")})}

  \item{...}{other arguments passed to
  \code{\link[base]{cat}}, e.g. \code{file} (this can be
  useful for batch-processing R scripts, e.g.
  \code{tidy.source(source = 'input.R', file =
  'output.R')})}
}
\value{
  A list with components \item{text.tidy}{The reformatted
  code as a character vector.} \item{text.mask}{The code
  containing comments, which are masked in assignments or
  with the weird operator.}
  \item{begin.comment,end.comment}{ identifiers used to
  mark the comments }
}
\description{
  This function has nothing to do with code optimization;
  it just returns parsed source code, but also tries to
  preserve comments, which is different with
  \code{\link[base]{parse}}. See `Details'.
}
\details{
  This function helps the users to tidy up their source
  code in a sense that necessary indents and spaces will be
  added, etc. See \code{\link[base]{parse}}. But comments
  will be preserved if \code{keep.comment = TRUE}.

  The method to preserve comments is to protect them as
  strings in disguised assignments. For example, there is a
  single line of comments in the source code:
  \preformatted{ # asdf}

  It will be first masked as

  \preformatted{.IDENTIFIER1 <- " # asdf.IDENTIFIER2"}

  which is a legal R expression, so
  \code{\link[base]{parse}} can deal with it and will no
  longer remove the disguised comments.  In the end the
  identifiers will be removed to restore the original
  comments, i.e. the strings \code{'.IDENTIFIER1 <- "'} and
  \code{'.IDENTIFIER2"'} are replaced with empty strings.

  ``Inline'' comments are handled differently: two spaces
  will be added before the hash symbol \code{#}, e.g.
  \preformatted{1+1# comments}

  will become \preformatted{1+1 # comments}

  Inline comments are first disguised as a weird operation
  with its preceding R code, which is essentially
  meaningless but syntactically correct!  For example,
  \preformatted{1+1 \%InLiNe_IdEnTiFiEr\% "# comments"}

  then \code{\link[base]{parse}} will deal with this
  expression; again, the disguised comments will not be
  removed. In the end, inline comments will be freed as
  well (remove the operator \code{\%InLiNe_IdEnTiFiEr\%}
  and surrounding double quotes).

  All these special treatments to comments are due to the
  fact that \code{\link[base]{parse}} and
  \code{\link[base]{deparse}} can tidy the R code at the
  price of dropping all the comments.
}
\note{
  When \code{keep.comment == TRUE}, \emph{all your double
  quotes in the comments will be replaced by single
  quotes!!} For example,

  \preformatted{1 + 1 # here is "comment"}

  will become \preformatted{1 + 1 # here is 'comment'}

  There are hidden options which can control the behaviour
  of this function: the argument \code{keep.comment} gets
  its value from \code{options('keep.comment')} by default;
  \code{keep.blank.line} from
  \code{options('keep.blank.line')}, \code{keep.space} from
  \code{options('keep.space')}, and \code{replace.assign}
  from \code{options('replace.assign')}. If these options
  are \code{NULL}, the default values will be \code{TRUE},
  \code{FALSE}, \code{FALSE} and \code{FALSE} respectively.

  Also note that if \code{keep.space} is \code{FALSE},
  single lines of long comments will be wrapped into
  shorter ones automatically. Otherwise, long comments will
  not be wrapped, so they may exceed the page margin, and
  \code{\\\\t} will be replaced with \code{\\t}. Roxygen
  comments will not be wrapped in any case.
}
\section{Warning}{
  The best strategy to avoid failure is to put comments in
  whole lines or after \emph{complete} R expressions. Here
  are some examples which could make
  \code{\link{tidy.source}} fail:

  \preformatted{1 + 2 +## comments after an incomplete line

  3 + 4}

  And the comments right after the curly brace will be
  moved to the next line, e.g.

  \preformatted{if (TRUE) {## comments

  }}

  will become \preformatted{if (TRUE) {

  ## comments

  }}
}
\examples{
library(formatR)

## use the 'text' argument
src = c("    # a single line of comments is preserved", 
    "1+1", "  ", "if(TRUE){", "x=1  # inline comments", "}else{", 
    "x=2;print('Oh no... ask the right bracket to go away!')}", 
    "1*3 # one space before this comment will become two!", "2+2+2    # 'short comments'", 
    "   ", "lm(y~x1+x2)  ### only 'single quotes' are allowed in comments", 
    "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line", 
    "\\t\\t## tabs/spaces before comments: use keep.space=TRUE to keep them", 
    "'a character string with \\t in it'", "# note tabs will be converted to spaces when keep.space = TRUE", 
    paste("## here is a", paste(rep("long", 20), collapse = " "), 
        "comment"))

## source code
cat(src, sep = "\\n")

## the formatted version
tidy.source(text = src)

## other options: preserve leading spaces
tidy.source(text = src, keep.space = TRUE)

## preserve blank lines
tidy.source(text = src, keep.blank.line = TRUE)

## discard comments!
tidy.source(text = src, keep.comment = FALSE)

## wanna see the gory truth??
tidy.source(text = src, output = FALSE)$text.mask


## tidy up the source code of image demo
x = file.path(system.file(package = "graphics"), "demo", 
    "image.R")

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
tidy.source(text = c("{if(TRUE)1 else 2; if(FALSE){1+1", 
    "## comments", "} else 2}"))
}
\author{
  Yihui Xie <\url{http://yihui.name}> with substantial
  contribution from Yixuan Qiu
  <\url{http://yixuan.cos.name}>
}
\references{
  \url{http://yihui.name/en/2010/04/formatr-farewell-to-ugly-r-code/}
}
\seealso{
  \code{\link[base]{parse}}, \code{\link[base]{deparse}},
  \code{\link[base]{cat}}
}
\keyword{IO}
