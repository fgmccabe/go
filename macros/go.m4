dnl NA_DETECT_GO(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl
dnl Description:
dnl   Detect the Go! installation
dnl
dnl Defines 
dnl   GODIR - installation directory 
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_GO,[

na_required=$1

AC_ARG_WITH(go,
[  --with-go[=dir]        indicate location of the go installation],
[na_go=${withval}],
[na_go=${GODIR}]
)

[na_parentprefix=`echo "$prefix"|sed -e 's%\(.\)/[^/]*/*$%\1%'`]

na_searchpath=${na_go}/bin:${PATH}:/opt/go/bin:$prefix/bin:$na_parentprefix/go/bin
AC_PATH_PROG(na_go,go,[not found],$na_searchpath)

if test "$na_go" = "not found" ; then
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([go not found in $na_searchpath
Run configure with --with-go to specify the Go
installation directory
])
  fi
fi

GODIR=`echo $na_go | sed -e s%/bin/go$%%` 
AC_SUBST(GODIR)

])