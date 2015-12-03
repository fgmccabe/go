#
# Makefile.am to create the go engine
#
SUBDIRS = Headers 

bin_PROGRAMS = go

go_SOURCES = main.c boot.c eval.c stats.c process.c suspend.c clock.c date.c \
	heap.c global.c gc.c float.c list.c string.c \
	dict.c escapes.c verify.c\
	exec.c signal.c error.c debug.c \
	args.c arith.c encoded.c writef.c misc.c\
	char.c fileio.c tcp.c dir.c envir.c put.c\
	program.c code.c \
	hashtable.c dynamic.c lockvar.c class.c call.c sha.c

INCLUDES = -I@top_srcdir@/Engine/Headers -I@top_srcdir@/Headers -I@ooiodir@/include '-DGODIR="@prefix@"'
go_LDFLAGS = @go_LDFLAGS@ 
go_LDADD = @ooiodir@/lib/libooio.la


