## Process with Automake to generate Makefile.in and eventually Makefile
## This will eventually be the Go! compiler

SUFFIXES = .go .gof .goc

noinst_PROGRAMS = ops esc

ops_SOURCES = ops.c

esc_SOURCES = esc.c
esc_LDADD = -L@ooiodir@/lib -looio

INCLUDES=-I@top_srcdir@/Headers -I@ooiodir@/include

include ${top_builddir}/go.Make

#
# Build the Go! in Go! compiler
#

ops.c ops.o: @top_srcdir@/Headers/operators.h

ops.go: ops
	./ops ops.go

esc.c:  @top_srcdir@/Headers/escapes.h

esc.go: esc  @top_srcdir@/Headers/escapes.h types.goc
	./esc esc.go

GO_SRCS = opts.go goc.go tokenizer.go abstract.go ops.go keywords.go \
	parse.go wff.go misc.go esc.go keywords.go errors.go types.go terms.go\
	findvars.go depends.go subtype.go macro.go parsetype.go sttypes.go \
	canon.go decode.go imports.go typeof.go


GO_EXTRA = typeaux.go types.go

GO_CODE = $(GO_SRCS:.go=.goc)

GO_F = $(GO_SRCS:.go=.gof)

CLEANFILES = ${GO_CODE} ${GO_F}

GOFLAGS = 
EXTRA_DIST = ${GO_SRCS} ${GO_EXTRA}

goc.goc: opts.goc tokenizer.goc errors.goc parse.goc abstract.goc wff.goc\
	terms.goc types.goc typeof.goc sttypes.goc

tokenizer.goc: errors.goc

misc.goc: 

opts.goc: misc.goc

abstract.goc: opts.goc ops.goc misc.goc

parse.goc: errors.goc ops.goc opts.goc abstract.goc tokenizer.goc

wff.goc: abstract.goc keywords.goc opts.goc errors.goc misc.goc esc.goc

types.goc: ops.goc misc.goc

terms.goc: abstract.goc types.goc ops.goc opts.goc

canon.goc: ops.goc types.goc opts.goc

findvars.goc: errors.goc opts.goc abstract.goc keywords.goc misc.goc types.goc

depends.goc: abstract.goc errors.goc misc.goc opts.goc terms.goc

parsetype.goc: sttypes.goc abstract.goc types.goc findvars.goc errors.goc\
	subtype.goc

sttypes.goc: esc.goc types.goc

subtype.goc: types.goc errors.goc misc.goc

decode.goc: terms.goc types.goc canon.goc

imports.goc: decode.goc types.goc terms.goc opts.goc misc.goc

quantify.goc: opts.goc terms.goc types.goc

typeof.goc: abstract.goc macro.goc misc.goc types.goc errors.goc depends.goc\
	findvars.goc subtype.goc keywords.goc terms.goc opts.goc parsetype.goc\
	imports.goc

all-local: goc.goc

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs ${prefix}/Goc
	(for XX in ${GO_CODE}; do\
	  $(INSTALL_SCRIPT) $${XX} ${prefix}/Goc; done)

test:
	(for XX in ${GO_SRCS}; do echo Testing $${XX}; go goc.goc -dT $${XX}; done)
