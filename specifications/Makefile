# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t -*-

PDFS= specifications.pdf
all:documents
documents:$(PDFS)
	open $(PDFS)
include ../tools/common.make

.PHONY::  questions tbd

-include specifications.d
clean::
	-rm specifications.d

questions:
	grep -nH -e '\<\(QUESTIONS\?\|HOW\)\>' *.txt

tbd:
	grep -nH -e '\<\(TBD\|GENERAL_DESCRIPTION\|DESCRIPTION_OF_THE_RESPONSIBILITIES_OF_THE_ENTITY\|LIST_OF_ATTRIBUTES\|LIST_OF_ASSOCIATIONS_WITH_OTHER_ENTITIES\)\>' *.txt
