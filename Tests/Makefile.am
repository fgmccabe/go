## Process with Automake to generate Makefile.in and eventually Makefile

SUFFIXES = .af .ah .go .gof .goc

include ${top_builddir}/go.Make

CLEANFILES = ${GO_CODE} ${GO_SRCS:.go=.gof}

#
# Build test programs
#

GO_SRCS = world.go ancestor.go person.go persontest.go plip.go bounded.go money.go diff.go naive.go\
	fact.go pingpong.go phil.go queen.go qsort.go tree.go hashtest.go onemsg.go\
	spiral.go env.go iden.go palin.go dyn.go dynchurn.go ls.go init.go shell.go\
	telnet.go xtest.go wait.go sync.go timeouts.go trans.go server.go stripsworld.go stacks.go\
	files.go celltest.go ifexp.go cases.go anon.go anon2.go bench.go\
	prstate.go directory.go dance.go syncwait.go google.go goweb.go num.go\
	comp.go valtest.go subclass.go parse.go suspsort.go\
	expand.go testlinda.go inner.go labels.go square.go sudoku.go \
	jsontest.go

GO_TSTS = world ancestor persontest plip onemsg bounded money diff naive bench fact pingpong phil\
	queen tree hashtest spiral env iden palin dyn dynchurn ls dance init shell xtest wait sync num\
	timeouts trans stripsworld stacks files celltest qsort comp ifexp cases anon\
	anon2 prstate syncwait google valtest subclass parse suspsort expand\
	testlinda inner labels sudoku jsontest

TST_FLAGS = -P.. -h 64

GO_CODE = $(GO_SRCS:.go=.goc)

persontest.goc: person.goc

dance.goc: directory.goc

sudoku.goc: square.goc

EXTRA_DIST = ${GO_SRCS} fact.xml

all-local: ${GO_CODE}

check-local: ${GO_CODE}
	(for Pk in $(GO_TSTS); do\
	  GO_DIR=${top_builddir}/ ${top_builddir}/Engine/go ${TST_FLAGS} $${Pk}||exit 1; done)

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs ${prefix}/Tests
	(for XX in ${GO_CODE}; do\
	  $(INSTALL_SCRIPT) $${XX} ${prefix}/Tests; done)
	(for XX in ${GO_SRCS}; do\
	  $(INSTALL_SCRIPT) $${XX} ${prefix}/Tests; done)
