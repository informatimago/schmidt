MODULES=specifications architecture analysis design sources tests
all::documents

include tools/common.make

help::
	@printf $(HELPFMT) "documents" "Build all the pdf documents."

documents::
	for module in $(MODULES) ; do $(MAKE) $(MFLAGS) -C $${module} documents ; done

help::
	@printf $(HELPFMT) "clean" "Delete spurious files."

clean::
	@find . -name \*.dx64fsl -exec rm -f {} \;
	@for module in $(MODULES) ; do $(MAKE) $(MFLAGS) -w -C $${module} clean ; done
