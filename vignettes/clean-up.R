#!/usr/bin/env Rscript

## automatic bib generation
library(tweakr)
write_citation(c('Rd2roxygen', 'knitr', 'formatR', 'pgfSweave'), file = 'formatR.bib')

