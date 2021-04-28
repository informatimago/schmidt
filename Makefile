MODULES=specifications architecture analysis design sources tests
PDFS=
all:: $(PDFS) documents

include tools/common.make

help::
	@printf $(HELPFMT) "documents" "Build all the pdf documents."
pdfs documents::
	for module in $(MODULES) ; do [ -d $${module} ] && $(MAKE) $(MFLAGS) -C $${module} documents ; done
pdfs documents::$(PDFS)
	open $(PDFS)

help::
	@printf $(HELPFMT) "clean" "Delete spurious files."

clean::
	@find . \( -name \*.dx64fsl -o -name --version.lock \) -exec rm -f {} \;
	for module in $(MODULES) ; do [ -d $${module} ] && $(MAKE) $(MFLAGS) -C $${module} clean ; done

# Generate PDF from org-mode document.
%.pdf:%.org
	-rm -f $@
	emacs --batch \
		--eval '(find-file "'$<'")' \
		--funcall org-latex-export-to-pdf \
		--kill
