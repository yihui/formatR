library(formatR)

## a messy R script
messy = system.file('format', 'messy.R', package = 'formatR')
tidy_source(messy)

## use the 'text' argument
src = readLines(messy)

## source code
cat(src, sep = '\n')

## the formatted version
tidy_source(text = src)

## preserve blank lines
tidy_source(text = src, blank = TRUE)

## indent with 2 spaces
tidy_source(text = src, indent = 2)

## discard comments!
tidy_source(text = src, comment = FALSE)

## wanna see the gory truth??
tidy_source(text = src, output = FALSE)$text.mask


## tidy up the source code of image demo
x = file.path(system.file(package = "graphics"), "demo", "image.R")

# to console
tidy_source(x)

# to a file
f = tempfile()
tidy_source(x, blank = TRUE, file = f)

## check the original code here and see the difference
file.show(x)
file.show(f)

## use global options
options(comment = TRUE, blank = FALSE)
tidy_source(x)

## if you've copied R code into the clipboard
if (interactive()) {
tidy_source("clipboard")
## write into clipboard again
tidy_source("clipboard", file = "clipboard")
}

## the if-else structure
tidy_source(text=c('{if(TRUE)1 else 2; if(FALSE){1+1',"## comments",'} else 2}'))
