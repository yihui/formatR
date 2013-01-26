# Makefile to use knitr for package vignettes

# put all targets here, separated by spaces
VIGNETTES= formatR.pdf index.html

all: $(VIGNETTES)

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.md

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "library(knitr); knit2pdf('$*.Rnw')"

%.html: %.Rmd
	$(R_HOME)/bin/Rscript -e "knitr::knit2html('$*.Rmd', options = c(markdown::markdownHTMLOptions(TRUE), 'toc'))"
