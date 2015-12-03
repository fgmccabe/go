#
# Makefile.am to build the Go! OpenSSL libraries
#

lib_LTLIBRARIES = libgossl.la

noinst_HEADERS = gossl.h gosslP.h

libgossl_la_SOURCES = gossl.c

INCLUDES = -I@top_srcdir@/Headers -I@top_srcdir@/Engine/Headers -I@ooiodir@/include '-DGODIR=@prefix@"'