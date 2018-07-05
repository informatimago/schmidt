MODULES=specifications architecture analysis design sources tests
PDFS= notes-for-emc.pdf
all::documents

include tools/common.make

help::
	@printf $(HELPFMT) "documents" "Build all the pdf documents."
documents::
	for module in $(MODULES) ; do $(MAKE) $(MFLAGS) -C $${module} documents ; done
documents::$(PDFS)
	open $(PDFS)

help::
	@printf $(HELPFMT) "clean" "Delete spurious files."

clean::
	@find . -name \*.dx64fsl -exec rm -f {} \;
	@for module in $(MODULES) ; do $(MAKE) $(MFLAGS) -w -C $${module} clean ; done

# Generate PDF from org-mode document.
%.pdf:%.org
	-rm -f $@
	emacs --batch \
		--eval '(find-file "'$<'")' \
		--funcall org-latex-export-to-pdf \
		--kill
