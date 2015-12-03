#
# Build the Go! RDF libraries
#
include ${top_builddir}/go.Make

SUFFIXES = .af .ah .go .goc

GO_SRCS =  rdfstore.go n3.go

GO_CODE = $(GO_SRCS:.go=.goc)

EXTRA_DIST = ${GO_SRCS}

CLEANFILES = ${GO_CODE} $(GO_SRCS:.go=.gof)

n3.goc: rdfstore.goc 

all-local: ${GO_CODE}

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/rdf
	(for XX in ${GO_CODE} ; do\
	  $(INSTALL_SCRIPT) $${XX} $(DESTDIR)${prefix}/rdf; done)
