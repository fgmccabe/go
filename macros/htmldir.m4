dnl NA_WITH_HTML
dnl
dnl Description:
dnl   Parse the --with-html option passed to configure to 
dnl   set the html documentation installation directory.
dnl
dnl Defines 
dnl   htmldir - html documentation installation directory
dnl
dnl Notes
dnl   Figure out where to put the html stuff

AC_DEFUN(NA_WITH_HTMLDIR,[

AC_ARG_WITH(htmldir,
[  --with-html[=PREFIX/html] indicate location of the html dir],
[htmldir=${withval}],
[htmldir='${prefix}/html'])

AC_SUBST(htmldir)

])