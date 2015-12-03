# 
# Rules for compiling april programs within the build tree
#
APRILDIR = file:////opt/april
APC = /opt/april/bin/apc
APRIL_PREAMBLE="\#!/opt/april/bin/april -C- -p$(GO_DIR)/BootCompiler $(APRIL_EXEC_FLAGS)"

%.aam: ${srcdir}/%.ap
	APRIL_DIR=$(APRILDIR) $(APC) $(APCFLAGS) -o $(basename $@).aam $<

%: ${srcdir}/%.ap
	APRIL_DIR=$(APRILDIR) APRIL_PREAMBLE=$(APRIL_PREAMBLE) $(APC) $(APCFLAGS) -o ${basename $@} -x $<

%.af: ${srcdir}/%.ap
	APRIL_DIR=$(APRILDIR) $(APC) $(APCFLAGS) -M -o $(basename $@).aam $<
