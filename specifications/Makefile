# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t -*-

PDFS= specifications.pdf
all:documents
documents:$(PDFS)
	open $(PDFS)

# include ../tools/common.make

# Generate PDF from org-mode document.
%.pdf:%.org
	-rm -f $@
	emacs --batch \
		--eval '(find-file "'$<'")' \
		--funcall org-latex-export-to-pdf \
		--kill

clean::
	- rm -f specifications.tex missfont.log


.PHONY::  questions tbd

questions:
	grep -nH -e '\<\(QUESTIONS\?\|HOW\)\>' *.txt

tbd:
	grep -nH -e '\<\(TBD\|GENERAL_DESCRIPTION\|DESCRIPTION_OF_THE_RESPONSIBILITIES_OF_THE_ENTITY\|LIST_OF_ATTRIBUTES\|LIST_OF_ASSOCIATIONS_WITH_OTHER_ENTITIES\)\>' *.txt
