#
# Build the Go Parser Generator
#

include ${top_builddir}/go.Make

GO_SRCS = gpgTypes.go grab.go gensets.go misc.go gengpgrules.go lr0.go closure.go\
	genparser.go abstract.go gpglex.go gpgrules.go lalr.go genrules.go gpg.go

GO_CODE = $(GO_SRCS:.go=.goc)

EXTRA_DIST = ${GO_SRCS}

CLEANFILES = ${GO_CODE} $(GO_SRCS:.go=.gof)

gpg.goc: gpgTypes.goc grab.goc gpgrules.goc genrules.goc gensets.goc\
	lr0.goc lalr.goc genparser.goc

gensets.goc: misc.goc

gpglex.go: gpglex.glx

gengpgrules.goc: gpgTypes.goc gensets.goc lr0.goc lalr.goc genparser.goc\
	gpglex.goc

gpgrules.go: gengpgrules.goc abstract.goc gpglex.goc
	GO_DIR=${top_builddir} $(GO) -P ${top_builddir} gpg.gengpgrules

lr0.goc: gpgTypes.goc gensets.goc misc.goc

lalr.goc: closure.goc gpgTypes.goc misc.goc gensets.goc 

all-local: gpg.goc

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/go/gpg
	(for XX in ${GO_CODE} ; do\
	  $(INSTALL_SCRIPT) $${XX} $(DESTDIR)${prefix}/go/gpg; done)
	$(INSTALL_SCRIPT) -m 0755 gpg $(DESTDIR)${prefix}/bin

