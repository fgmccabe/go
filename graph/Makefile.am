#
# Build the Go! graphics libraries
#
include ${top_builddir}/go.Make

SUFFIXES = .af .ah .go .goc

GO_SRCS =  matrix.go transform.go xlib.go widget.go graph.go event.go xlabel.go

GO_CODE = $(GO_SRCS:.go=.goc)

EXTRA_DIST = ${GO_SRCS}

CLEANFILES = ${GO_CODE} $(GO_SRCS:.go=.gof)

transform.goc: matrix.goc

xlib.goc: graph.goc 

event.goc: graph.goc xlib.goc

widget.goc: xlib.goc transform.goc graph.goc event.goc

xlabel.goc: widget.goc graph.goc event.goc xlib.goc

all-local: ${GO_CODE}

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/graph
	(for XX in ${GO_CODE} ; do\
	  $(INSTALL_SCRIPT) $${XX} $(DESTDIR)${prefix}/graph; done)
