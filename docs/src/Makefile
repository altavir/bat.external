.PHONY: all clean
PDF  = \
	bayes.pdf
BIBS = $(wildcard bib/*.bib)

## ================================================================
all   : ${PDF}

clean :
	rm -f *.aux *.log *.nav *.out *.snm *.vrb *.toc *.pdf


bayes.pdf : bayes.tex ${BIBS}
	pdflatex $< < /dev/null
	bibtex bayes.aux
	pdflatex $< < /dev/null
	pdflatex $< < /dev/null

## Generic rules
%.pdf : %.tex
	pdflatex $< < /dev/null
	pdflatex $< < /dev/null

%.pdf : %.svg
	inkscape $< --export-text-to-path --export-area-drawing --export-pdf $@
