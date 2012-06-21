#!/usr/bin/env Rscript

## automatic bib generation
library(knitr)
write_bib(c('Rd2roxygen', 'knitr', 'formatR', 'pgfSweave'), file = 'formatR.bib')

