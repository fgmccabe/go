#
# Build the Go Lexical generator
#

include ${top_builddir}/go.Make

SUFIXES = .go .goc

GO_SRCS = golex.go golextypes.go genregexp.go gendfa.go gentoken.go

GO_CODE = $(GO_SRCS:.go=.goc)

EXTRA_DIST = ${GO_SRCS}

CLEANFILES = ${GO_CODE} $(GO_SRCS:.go=.gof)

golex.goc: golextypes.goc genregexp.goc gendfa.goc gentoken.goc

genregexp.goc: golextypes.goc

gendfa.goc: golextypes.goc

gentoken.goc: golextypes.goc

all-local: golex.goc

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/go/golex
	(for XX in ${GO_CODE} ; do\
	  $(INSTALL_SCRIPT) $${XX} $(DESTDIR)${prefix}/go/golex; done)
	$(INSTALL_SCRIPT) -m 0755 golex $(DESTDIR)${prefix}/bin
