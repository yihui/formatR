#!/usr/bin/env Rscript

library(docopt)
library(formatR)

doc <- "
Usage:
  pre-commit-formatR.R [options] <files>...

Options:
  --no-comments         Remove comments. [default: FALSE]
  --no-blanks           Remove blank lines. [default: FALSE]
  --arrow               If FALSE, arrows will not be substituted for = signs. [default: FALSE]
  --pipe                If FALSE, pipes will not be changed. [default: FALSE]
  --brace-newline       If FALSE, braces will not be put on new lines. [default: FALSE]
  --indent=INDENT       Number of indents. [default: 4]
  --no-wrap             Skip wrapping the text. [default: TRUE]
  --width-cutoff=WIDTH  Text width cutoff. [default: 80]
  --args-newline        If TRUE, arguments are started on a new line after a function call. [default: FALSE]
"

args <- docopt(doc)

for (source_file in args[["files"]]) {
    formatR::tidy_source(source = source_file, file = source_file, comment = !args[["--no-comments"]],
        blank = !args[["--no-blanks"]], arrow = args[["--arrow"]], pipe = args[["--pipe"]],
        brace.newline = args[["--brace-newline"]], indent = as.integer(args[["--indent"]]),
        wrap = !args[["--no-wrap"]], width.cutoff = as.integer(args[["--width-cutoff"]]),
        args.newline = args[["--args-newline"]])$text

}
