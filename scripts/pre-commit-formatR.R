#!/usr/bin/env Rscript

library("R.utils")
library("formatR")

defaults <- c(
    source = "", comment = TRUE, blank = TRUE, arrow = FALSE, pipe = FALSE,
    brace.newline = FALSE, indent = 4, wrap = TRUE, width.cutoff = 80,
    args.newline = FALSE,
)

args <- cmdArgs()
str(args)

if ("--no_comments" %in% args)
{
    comments <- FALSE
} else
{
    comments <- TRUE
}

if ("--blank=TRUE" %in% args)
{
    blank <- TRUE
} else
{
    blank <- FALSE
}

files <- dir(pattern = "*.R$")
lapply(
    files, function(f) formatR::tidy_source(f, comment = comments, blank = blank)
)
