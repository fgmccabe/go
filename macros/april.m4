dnl NA_DETECT_APRIL(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl
dnl Description:
dnl   Detect the April installation
dnl
dnl Defines 
dnl   APRILDIR - installation directory 
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_APRIL,[

na_required=$1

AC_ARG_WITH(april,
[  --with-april[=dir]        indicate location of the april installation],
[na_april=${withval}],
[na_april=${APRILDIR}]
)

[na_parentprefix=`echo "$prefix"|sed -e 's%\(.\)/[^/]*/*$%\1%'`]

na_searchpath=${na_april}/bin:${PATH}:/opt/april/bin:$prefix/bin:$na_parentprefix/april/bin
AC_PATH_PROG(na_april,april,[not found],$na_searchpath)

if test "$na_april" = "not found" ; then
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([april not found in $na_searchpath
Run configure with --with-april to specify the April
installation directory
])
  fi
fi

APRILDIR=`echo $na_april | sed -e s%/bin/april$%%` 
AC_SUBST(APRILDIR)

AC_CACHE_VAL(word16,
[
AC_CHECK_SIZEOF(long,4)
AC_CHECK_SIZEOF(int,4)
AC_CHECK_SIZEOF(short int,2)

if test $ac_cv_sizeof_int = 2; then
  word16="int"
elif test $ac_cv_sizeof_short_int = 2; then
  word16="short int"
elif test $ac_cv_sizeof_long = 2; then
  word16="long"
else
  AC_MSG_ERROR([cant determine a 16 bit int type])
fi
])
AC_DEFINE_UNQUOTED(WORD16,$word16)

])