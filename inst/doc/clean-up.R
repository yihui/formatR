#!/usr/bin/env Rscript

## deal with LyX filename mangling
if (file.exists('formatR.tex')) {
    x = readLines('formatR.tex')
    idx = grep('\\\\documentclass', x)
    if (idx > 1) x = x[-(1:(idx-1))]
    idx = grep('\\\\bibliography|\\\\includegraphics', x)
    x[idx] = sub('\\{.*formatR_inst_doc_', '{', x[idx])
    writeLines(x, 'formatR.tex')
    file.rename('formatR.tex', 'formatR.Rnw')
}
unlink(sprintf('formatR.%s', c('aux', 'log', 'out', 'pdf')))
## now we can cheat Sweave :-)
