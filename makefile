################################################################################
# Copyright 2011
# Andrew Redd
# 11/23/2011
#
# Description of File:
# Makefile for knitr compiling
#
################################################################################
all:pdf # default rule DO NOT EDIT
################################################################################
MAINFILE := 3_16
RNWFILES :=
RFILES :=
TEXFILES :=
CACHEDIR := cache
FIGUREDIR := figures
LATEXMK_FLAGS :=
##### Explicit Dependencies #####
################################################################################
RNWTEX = $(RNWFILES:.Rnw=.tex)
ROUTFILES = $(RFILES:.R=.Rout)
RDATAFILES= $(RFILES:.R=.Rdata)
MAINTEX = $(MAINFILE:=.tex)
MAINPDF = $(MAINFILE:=.pdf)
ALLTEX = $(MAINTEX) $(RNWTEX) $(TEXFILES)

# Dependencies
$(RNWTEX): $(RDATAFILES)
$(MAINTEX): $(RNWTEX) $(TEXFILES)
$(MAINPDF): $(MAINTEX) $(ALLTEX)

.PHONY:pdf tex clean clearcache cleanall
pdf: $(MAINPDF)
tex: $(RDATAFILES) $(ALLTEX)

$(CACHEDIR):
	mkdir $(CACHEDIR)
$(FIGUREDIR):
	mkdir $(FIGUREDIR)

%.tex:%.Rnw
	Rscript \
	-e "library(knitr)" \
	-e "knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-')" \
	-e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
	-e "knitr::knit('$<','$@')"


%.R:%.Rnw
#	Rscript -e "Sweave('$^', driver=Rtangle())"
	Rscript -e "library(knitr); purl('$^', documentation=0)"
	sed -i 1,2d analysis.R

%.Rout:%.R
	R CMD BATCH "$^" "$@"

%.pdf: %.tex
	latexmk -pdf $<
clean:
	-latexmk -c -quiet $(MAINFILE).tex
	-latexmk -c -quiet $(MAINFILE)_handout.tex
	-rm -f $(MAINTEX) $(RNWTEX)
	-rm -rf $(FIGUREDIR)
#	-rm handout.log handout.aux
	-rm *tikzDictionary
#	-rm $(MAINPDF)
	-rm comment.cut
clearcache:
	-rm -rf cache
cleanall: clean clearcache

$(MAINFILE)_handout.pdf: $(MAINFILE)_handout.tex $(MAINFILE).pdf
	pdflatex $(MAINFILE)_handout.tex

open:
	open -a Skim.app $(MAINFILE).pdf
