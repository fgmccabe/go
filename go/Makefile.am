
#
# Build the Go! libraries
#
include ${top_builddir}/go.Make

SUFFIXES = .af .ah .go .goc

GO_SRCS =  stdlib.go cell.go queue.go io.go setlib.go boot.go stream.go pp.go\
	hash.go dynamic.go stdparse.go sort.go stack.go xml.go json.go \
	vector.go smtp.go\
	datelib.go unit.go debug.go debugger.go mbox.go do.go\
	http.go showable.go serialize.go queue.go opts.go\
	owldllite.go \
	profile.go profiler.go

GO_CODE = $(GO_SRCS:.go=.goc)

EXTRA_DIST = ${GO_SRCS}

CLEANFILES = ${GO_CODE} $(GO_SRCS:.go=.gof)

io.goc: cell.goc stdlib.goc

stream.goc: io.goc

stdlib.goc: stdlib.go
	@echo Compiling go! program $<
	GO_DIR=$(GO_DIR) $(GOC) -nostdlib $<

cell.goc: stdlib.goc

sort.goc: stdlib.goc

debugger.goc: stdlib.goc io.goc

debug.goc: io.goc xml.goc stdparse.goc showable.goc dynamic.goc

xml.goc: stdlib.goc io.goc hash.goc stdparse.goc showable.goc

stack.goc: cell.goc

datelib.goc: stdparse.goc

scomms.goc: stdlib.goc stdparse.goc io.goc encoded.goc

encoded.goc: io.goc serialize.goc

boot.goc: stdlib.goc

hash.goc: stdlib.goc

vector.goc: stdlib.goc hash.goc

profile.goc: io.goc stdlib.goc

profiler.goc: io.goc stdlib.goc xml.goc sort.goc stdparse.goc

dx.goc: stdlib.goc setlib.goc io.goc dynamic.goc xml.goc stdparse.goc

mbox.goc: stdlib.goc hash.goc queue.goc

http.goc: stdlib.goc xml.goc stdparse.goc datelib.goc setlib.goc

opts.goc: stdlib.goc io.goc

json.goc: stdlib.goc stdparse.goc

smtp.goc: stdlib.goc io.goc

all-local: ${GO_CODE}

debug: ${GO_CODE}
	GOFLAGS=-g make all-local

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/go
	(for XX in ${GO_CODE} ; do\
	  $(INSTALL_SCRIPT) $${XX} $(DESTDIR)${prefix}/go; done)
