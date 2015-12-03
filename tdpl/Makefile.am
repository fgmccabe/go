include ${top_builddir}/go.Make

SUFIXES = .go .goc

GO_SRCS = ctypes.go dcgparse.go canon.go cnf.go

tdpl.goc: dcgparse.goc ctypes.goc canon.goc

canon.goc: ctypes.goc

cnf.goc: canon.goc ctypes.goc

dcgparse.goc: ctypes.goc

GO_CODE = $(GO_SRCS:.go=.goc)

EXTRA_DIST = ${GO_SRCS}

CLEANFILES = ${GO_CODE} $(GO_SRCS:.go=.gof)

all-local: tdpl.goc

