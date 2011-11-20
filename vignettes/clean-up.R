#!/usr/bin/env Rscript

## automatic bib generation
library(tweakr)
write_citation(c('Rd2roxygen', 'knitr', 'formatR', 'pgfSweave'), file = 'formatR.bib')

## deal with LyX filename mangling
if (file.exists('formatR.tex')) {
    x = readLines('formatR.tex')
    idx = grep('\\\\documentclass', x)
    if (idx > 1) x = x[-(1:(idx-1))]
    idx = grep('\\\\bibliography|\\\\includegraphics', x)
    x[idx] = sub('\\{.*formatR_vignettes_', '{', x[idx])
    writeLines(x, 'formatR.tex')
    file.rename('formatR.tex', 'formatR.Rnw')
}
unlink(sprintf('formatR.%s', c('aux', 'log', 'out', 'pdf')))
## now we can cheat Sweave :-)
