library(formatR)

## a messy R script
messy = system.file('format', 'messy.R', package = 'formatR')
tidy.source(messy)

## use the 'text' argument
src = readLines(messy)

## source code
cat(src, sep = '\n')

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
tidy.source(text=c('{if(TRUE)1 else 2; if(FALSE){1+1',"## comments",'} else 2}'))
