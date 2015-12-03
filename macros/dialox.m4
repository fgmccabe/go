dnl NA_DETECT_DIALOX(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl
dnl Description:
dnl   Detect the Dialox installation
dnl
dnl Defines 
dnl   DXDIR - installation directory 
dnl   DXINCLUDE - dialox include directory
dnl   HAVE_DIALOX - Automake conditional variable
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_DIALOX,[

na_required=$1
na_result=""
na_dialox=""

AC_ARG_WITH(dialox,
[  --with-dialox[=dir]       indicate location of the dialox installation],
[na_dialox=${withval}],
)

if test -z ${na_dialox} ; then
  [na_parentprefix=`echo "$prefix"|sed -e 's%\(.\)/[^/]*/*$%\1%'`]
  na_searchpath=:${PATH}:/opt/dialox/bin:/opt/dx/bin:$na_parentprefix/dialox/bin:$na_parentprefix/dx/bin
else
  na_searchpath=${na_dialox}/bin
fi

AC_PATH_PROG(na_dialox,dx,[not found],$na_searchpath)

if test "$na_dialox" = "not found" ; then
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([dx not found in $na_searchpath
Run configure with --with-dialox to specify the Dialox
installation directory
])
  fi
fi

DXDIR=`echo $na_dialox | sed -e s%/bin/dx$%%` 
DXINCLUDE=$DXDIR/include

AC_SUBST(DXDIR)
AC_SUBST(DXINCLUDE)
if test -d "$DXDIR" ; then HAVE_DX=true ; else HAVE_DX=false ; fi
AM_CONDITIONAL(HAVE_DIALOX, test x$HAVE_DX = xtrue)
AC_SUBST(HAVE_DX)

])
