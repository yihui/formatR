# Makefile to use knitr for package vignettes

# put all targets here, separated by spaces
VIGNETTES= formatR.pdf index.html

all: $(VIGNETTES)

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.md

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2pdf('$*.Rnw') else tools::texi2pdf('$*.tex')"

%.html: %.Rmd
	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2html('$*.Rmd', options = c(markdown::markdownHTMLOptions(TRUE), 'toc'))"
