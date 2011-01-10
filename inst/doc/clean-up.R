#!/usr/bin/env Rscript

## deal with LyX filename mangling
x = readLines('formatR.tex')
idx = grep('\\\\documentclass', x)
if (idx > 1) x = x[-(1:(idx-1))]
idx = grep('\\\\bibliography|\\\\includegraphics', x)
x[idx] = sub('\\{.*formatR_inst_doc_', '{', x[idx])
writeLines(x, 'formatR.tex')
file.rename('formatR.tex', 'formatR.Rnw')
## now we can cheat Sweave :-)
