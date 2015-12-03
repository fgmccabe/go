dnl NA_DETECT_APRILDEP(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl
dnl Description:
dnl   Detect if the aprildep automatic dependency generator is 
dnl   available
dnl
dnl Defines 
dnl   APRILDEP - installation directory 
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_APRILDEP,[

na_required=$1
na_result=""

AC_ARG_WITH(aprildep,
[  --with-aprildep[=path]    indicate directory containing aprildep executable],
[na_aprildep=${withval}],
)

na_searchpath=${na_aprildep}:${PWD}/macros/bin:${PATH}
AC_PATH_PROG(na_aprildep,aprildep,[not found],$na_searchpath)

if test "$na_aprildep" = "not found" ; then
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([aprildep not found in $na_searchpath
Run configure with --with-aprildep to specify the aprildep
installation directory
])
  else
    na_aprildep=:
  fi
fi

APRILDEP=$na_aprildep
AC_SUBST(APRILDEP)


])



