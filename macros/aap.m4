dnl NA_DETECT_AAP(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl
dnl Description:
dnl   Detect the April Agent Platform installation
dnl
dnl Defines 
dnl   AAPDIR - installation directory 
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_AAP,[

na_required=$1
na_result=""

AC_ARG_WITH(aap,
[  --with-aap[=dir]        Indicate location of the AAP installation],
[AAPDIR=${withval}],
[AAPDIR=guess]
)

if test "x$AAPDIR" = "xguess" ; then
  [na_parentprefix=`echo "$prefix"|sed -e 's%\(.\)/[^/]*/*$%\1%'`]
  na_searchpath=${PATH}:/opt/aap/bin:$prefix/bin:$na_parentprefix/aap/bin
  AC_PATH_PROG(na_aapconfig,aap-config,[not found],$na_searchpath)
  if test -x "$na_aapconfig" ; then
     AAPDIR=`$na_aapconfig -p`
  fi
fi

AC_MSG_CHECKING(for aap directory)

na_result="${AAPDIR}"
if ! test -f "${AAPDIR}/include/libfipa.ah" ; then
  na_result="not found"
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([${AAPDIR}/include/libfipa.ah not found.
Run configure with --with-aap to specify the aap installation directory
])
  fi
fi

AC_MSG_RESULT($AAPDIR)
AC_SUBST(AAPDIR)

])
