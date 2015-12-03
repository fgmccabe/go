# 
# Rules for compiling go programs within the build tree
#
GODIR = ${top_builddir}/go
GOC = ${top_builddir}/Compiler/goc

%.goc: ${srcdir}/%.go ${GOC}
	@echo Were compiling go! program $? ":-)"
	GODIR=$(GODIR) $(GOC) $(GOFLAGS) $<

%.gof: ${srcdir}/%.go ${GOC}
	@echo Were compiling go! program $? ":-)"
	GODIR=$(GODIR) $(GOC) +M $(GOFLAGS) $<

