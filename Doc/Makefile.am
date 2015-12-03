# Build the pdf etc of the reference manual
SUBDIRS = QuickRef

SUFFIXES = .tex

noinst_PROGRAMS = mnem ops esc

mnem_SOURCES = mnem.c

ops_SOURCES = ops.c

esc_SOURCES = esc.c

INCLUDES=-I@top_srcdir@/Headers -I@ooiodir@/include

GOREF_SOURCES = go-ref.tex introduction.tex grammar.tex types.tex\
            expressions.tex goals.tex actions.tex grammarrule.tex canonical.tex compile.tex\
            arith.tex stdlib.tex chars.tex string.tex dynamic.tex io.tex errorcodes.tex\
	    howitworks.tex misc.tex gcl.tex time.tex lo.tex

EXTRA_DIST = $(GOREF_SOURCES) LogicBib.bib shorthand.pdf prog.sty fgm.sty nap.sty go-ref.glo\
	     bussproofs.sty

mnem.c mnem.o: @top_srcdir@/Headers/instructions.h

mnem.tex: @top_srcdir@/Headers/instructions.h
	./mnem mnem.tex

ops.c ops.o: @top_srcdir@/Headers/operators.h

ops.tex: @top_srcdir@/Headers/operators.h
	./ops ops.tex

esc.c esc.o: @top_srcdir@/Headers/escapes.h

esc.tex: @top_srcdir@/Headers/escapes.h
	./esc esc.tex

grammar.tex:  ops.tex

howitworks.tex: esc.tex mnem.tex

all-local: go-ref.pdf

go-ref.pdf: ${GOREF_SOURCES} mnem.tex ops.tex esc.tex
	pdflatex ${srcdir}/go-ref.tex
	pdflatex ${srcdir}/go-ref.tex
	makeindex ${srcdir}/go-ref.idx
	pdflatex ${srcdir}/go-ref.tex

go-ref.nls: ${GOREF_SOURCES}
	makeindex go-ref.nlo -s nomencl.ist -o go-ref.nls

go-ref.bbl: LogicBib.bib ${GOREF_SOURCES}
	bibtex ${srcdir}/go-ref.tex


CLEANFILES = *.aux *.log

install-data-local: go-ref.pdf
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/Doc
	$(INSTALL_SCRIPT) go-ref.pdf $(DESTDIR)${prefix}/Doc
