#!/bin/sh
# This file is a modified version of autogen.sh in gtk+
# Run this to generate all the initial makefiles, etc.

aclocal -I macros
autoheader
automake -a
autoconf

case "$1" in
    "-r"|"--rebuild")
	./config.status --recheck
	./config.status
	;;

    *) 
	rm -f config.cache
	echo "$0: configure $*"
	./configure $*
	;;
esac

echo "Now type 'make' to compile"
