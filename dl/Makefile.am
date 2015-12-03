#
# Build the DL library
#

include ${top_builddir}/go.Make

GO_SRCS = dl.go parse.go terms.go
GO_CODE = $(GO_SRCS:.go=.goc) dllex.goc
CLEANFILES = ${GO_CODE} $(GO_SRCS:.go=.gof)

GOLEX_OPTS=-p dl.dllex 

parse.goc: terms.goc dllex.goc

dl.goc: terms.goc parse.goc

all-local: ${GO_CODE}

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/dl
	(for XX in ${GO_CODE} ; do\
	  $(INSTALL_SCRIPT) $${XX} $(DESTDIR)${prefix}/dl; done)
