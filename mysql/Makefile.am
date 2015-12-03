#
# Make the mysql plugin for Go!
#

include ${top_builddir}/go.Make

SUFIXES = .go .goc

GO_SRC = misc.go packet.go session.go errors.go parse.go sqltypes.go

misc.goc: packet.goc

mysql.goc: sqltypes.goc parse.goc packet.goc misc.goc

packet.goc: sqltypes.goc

parse.goc: sqltypes.goc

session.goc: misc.goc packet.goc sqltypes.goc

GO_CODE=$(GO_SRC:.go=.goc)

CLEANFILES = ${GO_CODE} $(GO_SRCS:.go=.gof)

all-local: ${GO_CODE} 

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/go/mysql
	(for XX in ${GOC_FILES} ; do\
	  $(INSTALL_SCRIPT) $${XX} ${prefix}/go/mysql; done)
