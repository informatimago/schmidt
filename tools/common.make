# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t -*-

ifeq ($(shell grep -q -s informatimago /etc/resolv.conf && echo yes || echo no),yes)
LISP             ?= ccl
LISP_FLAGS       ?= --no-init
LISP_LOAD_OPTION ?= --load
LISP_EVAL_OPTION ?= --eval
#PDF_VIEWER       ?= /opt/local/bin/xpdf
JAVA             ?= java
PDF_VIEWER       ?= open
WGET             ?= wget
WGET_OPTIONS     ?=
SED              ?= gsed
else
LISP             ?= /usr/local/bin/sbcl
LISP_FLAGS       ?= --no-sysinit --no-userinit
LISP_LOAD_OPTION ?= --load
LISP_LOAD_OPTION ?= --eval
JAVA             ?= /usr/bin/java
PDF_VIEWER       ?= /usr/bin/xpdf
WGET             ?= /usr/bin/wget
WGET_OPTIONS     ?=
SED              ?= sed
endif

TOP               = $(shell git rev-parse --show-toplevel)
MAKE_RST_DEPENDS ?= $(TOP)/tools/make-rst-depends
RSTPRE           ?= $(TOP)/tools/rstpre
RSTUML           ?= $(TOP)/tools/rstuml
PLANTUML         ?= $(TOP)/tools/plantuml.jar
PLANTUML_OPTIONS ?= -DPLANTUML_LIMIT_SIZE=8192  -Djava.awt.headless=true -Dapple.awt.UIElement=false

HELPFMT          ?= "$(shell basename $(MAKE)) %-20s \# %s\n"
STYLESHEETS_DIR = /usr/share/xml/docbook/stylesheet/nwalsh

.PHONY:: view clean clean-pdf clean-pdfs checkout-pdf checkout-pdfs documents

view:$(PDFS)
	@if [ $(PDF_VIEWER) = open ] ; then \
		$(PDF_VIEWER) $^ ;\
	 else \
		for i in $^ ; do \
			$(PDF_VIEWER) $$i & \
		done ;\
	 fi

clean:: clean-pdf clean-png

clean-pdf clean-pdfs:
	-rm -f $(PDFS)

clean-png clean-pngs:
	-rm -f *.png

# Generate the dependencies of restructuredText documents (includes
# and images).  Note: this should be done better by a script, since
# included files may themselves include other files or images.
%.d:%.txt
	@ printf '// Texting %s\n' $<
	@ $(MAKE_RST_DEPENDS) $< > $@

# Generate PDF from reStructuredText document.
%.pdf:%.txt
	rst2pdf -o $@ $<

# Object diagrams = Class diagrams.
o-%.png:o-%.puml
	@ printf '// Diagramming %s\n' $<
	@ $(JAVA) $(PLANTUML_OPTIONS) -jar $(PLANTUML) $< > $@

# State diagrams = Activity diagrams.
s-%.png:s-%.puml
	@ printf '// Diagramming %s\n' $<
	@ $(JAVA) $(PLANTUML_OPTIONS) -jar $(PLANTUML) $< > $@

# Use-case diagrams.
u-%.png:u-%.puml
	@ printf '// Diagramming %s\n' $<
	@ $(JAVA) $(PLANTUML_OPTIONS) -jar $(PLANTUML) $< > $@

# Activity diagrams
a-%.png:a-%.puml
	@ printf '// Diagramming %s\n' $<
	@ $(JAVA) $(PLANTUML_OPTIONS) -jar $(PLANTUML) $< > $@

# Global class diagrams
cd-%.png:cd-%.puml
	@ printf '// Diagramming %s\n' $<
	@ $(JAVA) $(PLANTUML_OPTIONS) -jar $(PLANTUML) $< > $@

# Sequence diagrams
sd-%.png:sd-%.puml
	@ printf '// Diagramming %s\n' $<
	@ $(JAVA) $(PLANTUML_OPTIONS) -jar $(PLANTUML) $< > $@

checkout-pdf checkout-pdfs:clean-pdf
	git checkout $(PDFS)


## Example, using rstpre and rstuml:
##
# .SUFFIXES: .in1 .txt .rst .pdf .xml .odt .html
#
# # All the preprocessing of the documents into a rst file should go here:
# %.rst:%.txt
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Preparing $$i" ;\
# 	cd "$$d" \
# 	&& $(RSTPRE) "$$f" \
# 	&& mv "$${f/.txt/.rst}"  "$${f/.txt/.in1}" \
# 	&& echo "Processing UML in $${i/.txt/.in1}" \
# 	&& $(RSTUML) "$${f/.txt/.in1}"
#
# # Following are the rules to process rst files into the target formats:
# %.pdf:%.rst
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Producing $${i/.rst/.pdf}" ;\
# 	cd "$$d" \
# 	&& $(RST2PDF)  $$( [ -r $${f/.rst/.style} ] && echo -s $${f/.rst/.style} )  -o "$${f/.rst/.pdf}"  "$$f"
# # --verbose --very-verbose
#
# %.odt:%.rst
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Producing $${i/.rst/.odt}" ;\
# 	cd "$$d" \
# 	&& $(RST2ODT) $$( [ -r $${f/.rst/.style} ] && echo -s $${f/.rst/.style} ) "$$f" "$${f/.rst/.odt}"
#
# %.html:%.rst
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Producing $${i/.rst/.html}" ;\
# 	cd "$$d" \
# 	@f="$<" \
# 	&& $(RST2HTML) $$( [ -r $${f/.rst/.style} ] && echo -s $${f/.rst/.style} ) "$$f" "$${f/.rst/.html}"
