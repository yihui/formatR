library(formatR)

## use the 'text' argument
src = c("    # a single line of comments is preserved",
'1+1', '  ', 'if(TRUE){',
"x=1  # inline comments", '}else{',
"x=2;print('Oh no... ask the right bracket to go away!')}",
'1*3 # one space before this comment will become two!',
"2+2+2    # 'short comments'", "   ",
"lm(y~x1+x2, data=data.frame(y=rnorm(100),x1=rnorm(100),x2=rnorm(100)))  ### only 'single quotes' are allowed in comments",
"1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line",
"\t\t## tabs/spaces before comments: use keep.space=TRUE to keep them",
"'a character string with \t in it'",
"# note tabs will be converted to spaces when keep.space = TRUE",
paste("## here is a", paste(rep("long", 20), collapse = ' '), "comment"))

## source code
cat(src, sep = '\n')

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
