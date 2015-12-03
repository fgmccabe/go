## Process with Automake to generate Makefile.in and eventually Makefile
## This will eventually be the Go! compiler

SUFFIXES = .af .ah .go .gof .goc

noinst_PROGRAMS = instr mnem esc esccode ops

instr_SOURCES = instr.c

mnem_SOURCES = mnem.c

ops_SOURCES = ops.c

esc_SOURCES = esc.c
esc_LDADD = -L@ooiodir@/lib -looio

esccode_SOURCES = esccode.c

INCLUDES=-I@top_srcdir@/Headers -I@ooiodir@/include

# 
# Rules for compiling go programs within the build tree
#
GODIR = ${topdir}/go
GOC = ${top_builddir}/BootCompiler/goc

%.goc: ${srcdir}/%.go ${GOC}
	@echo Were compiling go! program $? ":-)"
	GODIR=$(GODIR) $(GOC) $(GOFLAGS) $<

%.gof: ${srcdir}/%.go ${GOC}
	@echo Were compiling go! program $? ":-)"
	GODIR=$(GODIR) $(GOC) +M $(GOFLAGS) $<

include ${top_builddir}/BootCompiler/april.Make
include ${top_builddir}/BootCompiler/go.Make

APRIL_EXEC_FLAGS= -h2048

BOOT_SRCS = abstract.ap analyse.ap canon.ap canonicalize.ap\
	canontype.ap canonlabel.ap canonclass.ap\
	codegen.ap compile.ap compvar.ap debug.ap decode.ap\
	depends.ap dict.ap display.ap dynamic.ap ecode.ap encode.ap\
	errors.ap esc.ap estimate.ap findvars.ap goc.ap golex.ap grammar.ap\
	ground.ap hash.ap import.ap index.ap keywords.ap macro.ap misc.ap\
	mnem.ap ops.ap parsetype.ap peephole.ap quantify.ap refactor.ap\
	st-types.ap subtype.ap typeof.ap types.ap url.ap wff.ap canonmeta.ap

BOOT_CODE = $(BOOT_SRCS:.ap=.aam) 

BOOT_AF = $(BOOT_SRCS:.ap=.af)

noinst_HEADERS = go_tok.ah terms.ah go.ah xtree.ah canon.ah

EXTRA_DIST = golex.alx go_tok.ah instr.ah dectree.ap\
	ops.ah terms.ah \
	${BOOT_SRCS}

instr.o instr.c:  @top_srcdir@/Headers/instructions.h

instr.ah: @top_srcdir@/Headers/instructions.h
	./instr instr.ah

mnem.c mnem.o: @top_srcdir@/Headers/instructions.h

mnem.ap: @top_srcdir@/Headers/instructions.h 
	./mnem mnem.ap

mnem.aam mnem.af: ecode.af

esc.c:  @top_srcdir@/Headers/escapes.h @top_srcdir@/Headers/xlibescapes.h

esc.ap: esc
	./esc > esc.ap

esccode.c:  @top_srcdir@/Headers/escapes.h @top_srcdir@/Headers/xlibescapes.h

ecode.ap: @top_srcdir@/Headers/escapes.h @top_srcdir@/Headers/xlibescapes.h
	./esccode ecode.ap

ops.c ops.o: @top_srcdir@/Headers/operators.h

ops.ap: @top_srcdir@/Headers/operators.h
	./ops ops.ap

encode.aam encode.af: misc.af canon.af types.af

decode.aam decode.af: misc.af types.af

misc.aam misc.af: url.af

goc.aam: goc.ap misc.af url.af grammar.af\
	abstract.af display.af wff.af errors.af\
	types.af typeof.af st-types.af\
	canonicalize.af canon.af go.ah canon.ah\
	compile.af encode.af ground.af

abstract.aam abstract.af: ops.af misc.af

url.aam url.af: golex.af

golex.ap: golex.alx go_tok.ah
	alex golex.alx

st-types.aam st-types.af: esc.af

vars.aam vars.af: misc.af display.af

types.aam types.af: abstract.af misc.af errors.af ops.af

findvars.aam findvars.af: abstract.af errors.af types.af st-types.af misc.af display.af

subtype.aam subtype.af: errors.af misc.af ops.af types.af

typeof.aam typeof.af: abstract.af display.af errors.af subtype.af\
	findvars.af depends.af misc.af quantify.af import.af keywords.af macro.af types.af\
	parsetype.af

parsetype.aam parsetype.af: types.af subtype.af errors.af misc.af findvars.af

quantify.aam quantify.af: display.af errors.af types.af misc.af findvars.af

grammar.aam grammar.af: ops.af errors.af url.af abstract.af

import.aam import.af: errors.af decode.af url.af misc.af types.af

wff.aam wff.af: errors.af misc.af keywords.af st-types.af

depends.aam depends.af: abstract.af errors.af misc.af 

display.aam display.af: ops.af misc.af terms.ah abstract.af types.af

displayparse.aam displayparse.af: ops.af misc.af 

analyse.aam analyse.af: ground.af misc.af canon.af dict.af st-types.af

ground.aam ground.af: display.af types.af misc.af

errors.aam errors.af: misc.af

index.aam index.af: hash.af canon.af errors.af ground.af codegen.af misc.af\
	hash.af compvar.af

refactor.aam refactor.af: canon.af ground.af misc.af

canontype.aam canontype.af: st-types.af misc.af

canon.aam canon.af: ops.af misc.af st-types.af display.af types.af

canonicalize.aam canonicalize.af: errors.af misc.af canon.af ground.af \
	types.af debug.af findvars.af st-types.af display.af\
	canonlabel.af canonclass.af

canonlabel.aam canonlabel.af: misc.af canon.af types.af canon.ah refactor.af\
	dynamic.af display.af errors.af 

canonclass.aam canonclass.af: errors.af misc.af canon.af ground.af\
	types.af debug.af findvars.af st-types.af display.af\
	canonlabel.af canonmeta.af

# dectree.af 

dirtree.aam dirtree.af: xtree.ah misc.af canon.af

dectree.aam dectree.af: misc.af canon.af dirtree.af xtree.ah

debug.aam debug.af: misc.af canon.af errors.af refactor.af types.af canontype.af\
	canonlabel.af

compvar.aam compvar.af: misc.af dict.af display.af errors.af instr.ah 

comptype.aam comptype.af: dict.af display.af errors.af \
	instr.ah compvar.af codegen.af canon.af ground.af

peephole.af peephole.aam: instr.ah misc.af

estimate.af estimate.aam: canon.af errors.af ground.af hash.af

compile.aam compile.af: misc.af dict.af canon.af errors.af\
	instr.ah codegen.af analyse.af ecode.af mnem.af compvar.af\
	peephole.af ground.af index.af estimate.af types.af debug.af

codegen.aam codegen.af: instr.ah display.af misc.af

dict.aam dict.af:  misc.af display.af errors.af

%.aam %.af: ${srcdir}/%.ap
	@echo Compiling april program $@ ":-)"
	APRIL_DIR=$(APRILDIR) $(APC) $(APCFLAGS) -M -o $(basename $@).aam $<

CLEANFILES = ${BOOT_CODE} ${BOOT_AF}

all-local: goc.aam

install-data-local: ${BOOT_CODE}
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/BootCompiler
	(for XX in ${BOOT_CODE}; do\
	  $(INSTALL_SCRIPT) $${XX} $(DESTDIR)${prefix}/BootCompiler; done)
	$(INSTALL_SCRIPT) -m 0755 goc $(DESTDIR)${prefix}/bin
