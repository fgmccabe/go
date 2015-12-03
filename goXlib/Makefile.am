#
# Makefile.am to build the Go! Xlib libraries
#

lib_LTLIBRARIES = libxgo.la

noinst_HEADERS = goX.h

libxgo_la_SOURCES = xdisplay.c xcolor.c xwindow.c xevent.c xgc.c xdraw.c\
	xtext.c

INCLUDES = @X_CFLAGS@ -I@top_srcdir@/Headers -I@top_srcdir@/Engine/Headers -I@ooiodir@/include '-DGODIR=@prefix@"'