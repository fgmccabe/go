# Build the pdf etc of the reference manual
SUFFIXES = .tex

noinst_PROGRAMS = ops esc

ops_SOURCES = ops.c

esc_SOURCES = esc.c

INCLUDES=-I@top_srcdir@/Headers -I@ooiodir@/include

GOREF_SOURCES = quick.tex introduction.tex grammar.tex types.tex\
            expressions.tex goals.tex actions.tex grammarrule.tex canonical.tex compile.tex\
            arith.tex stdlib.tex chars.tex string.tex dynamic.tex io.tex errorcodes.tex\
	    misc.tex gcl.tex time.tex lo.tex

EXTRA_DIST = $(GOREF_SOURCES) LogicBib.bib shorthand.pdf prog.sty fgm.sty nap.sty quick.glo\
	     bussproofs.sty

ops.c ops.o: @top_srcdir@/Headers/operators.h

ops.tex: @top_srcdir@/Headers/operators.h
	./ops ops.tex

esc.c esc.o: @top_srcdir@/Headers/escapes.h

esc.tex: @top_srcdir@/Headers/escapes.h
	./esc esc.tex

grammar.tex:  ops.tex

all-local: quick.pdf

quick.pdf: ${GOREF_SOURCES} mnem.tex ops.tex esc.tex
	pdflatex ${srcdir}/quick.tex
	pdflatex ${srcdir}/quick.tex
	makeindex ${srcdir}/quick.idx
	pdflatex ${srcdir}/quick.tex

quick.nls: ${GOREF_SOURCES}
	makeindex quick.nlo -s nomencl.ist -o quick.nls

quick.bbl: LogicBib.bib ${GOREF_SOURCES}
	bibtex ${srcdir}/quick.tex


CLEANFILES = *.aux *.log

install-data-local: quick.pdf
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/Doc
	$(INSTALL_SCRIPT) quick.pdf $(DESTDIR)${prefix}/Doc
