.PHONY: all fig clean
PDF  = \
	problems-set.pdf
BIBS = $(wildcard *.bib)
FIGS = $(wildcard fig/*.pdf)

## ================================================================
all   : fig ${PDF}
	@echo ${PDF}

clean :
	rm -f *.aux *.log *.nav *.out *.snm *.vrb *.toc *.pdf


## Generic rules
%.pdf : %.tex
	pdflatex $< < /dev/null
	pdflatex $< < /dev/null

%.pdf : %.svg
	inkscape $< --export-text-to-path --export-area-drawing --export-pdf $@
