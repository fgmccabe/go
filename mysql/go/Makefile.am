#
# Build the mysql Go! libraries
#
include ${top_builddir}/go.Make

SUFFIXES = .af .ah .go .gof .goc

GOFLAGS = -dest ext/mysql/go

GO_SRCS = mysql.go

GOF_FILES = $(GO_SRCS:.go=.gof)
GO_CODE = $(GO_SRCS:.go=.goc)

EXTRA_DIST = ${GO_SRCS}

CLEANFILES = ${GO_CODE} ${GOF_FILES}

all-local: ${GO_CODE} ${GOF_FILES}

install-data-local: ${GO_CODE} ${GOF_FILES}
	-${top_srcdir}/mkinstalldirs ${prefix}/ext/mysql/go
	(for XX in ${GOF_FILES} ${GO_CODE}; do\
	  $(INSTALL_SCRIPT) $${XX} ${prefix}/ext/mysql/go; done)



