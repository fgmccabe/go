## Process this with automake to generate Makefile.in

SUBDIRS = macros Headers Engine BootCompiler go golex gpg\
	rdf dl mysql tdpl Tests GoCompiler Doc 

lisp_LISP = go.el godebug.el

go.elc: godebug.elc

ELCFILES = go.elc godebug.elc

EXTRA_DIST = go.spec go.spec.in go.el godebug.el

rpm:
	rpm -bb go.spec





