dnl NA_DETECT_WEBSTREAM(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl
dnl Description:
dnl   Detect the WebStream installation
dnl
dnl Defines 
dnl   WEBSTREAMDIR - installation directory 
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_WEBSTREAM,[

na_required=$1
na_result=""

AC_ARG_WITH(webstream,
[  --with-webstream[=dir]        Indicate location of the WEBSTREAM installation],
[WEBSTREAMDIR=${withval}],
[WEBSTREAMDIR=guess]
)

if test "x$WEBSTREAMDIR" = "xguess" ; then
  [na_parentprefix=`echo "$prefix"|sed -e 's%\(.\)/[^/]*/*$%\1%'`]
  na_searchpath=${PATH}:/opt/webstream/bin:$prefix/bin:$na_parentprefix/webstream/bin
  AC_PATH_PROG(na_webstreamconfig,webstream-config,[not found],$na_searchpath)
  if test -x "$na_webstreamconfig" ; then
     WEBSTREAMDIR=`$na_webstreamconfig -p`
  fi
fi

AC_MSG_CHECKING(for webstream directory)

na_result="${WEBSTREAMDIR}"
if ! test -f "${WEBSTREAMDIR}/include/webstream.ah" ; then
  na_result="not found"
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([${WEBSTREAMDIR}/include/webstream.ah not found.
Run configure with --with-webstream to specify the webstream installation directory
])
  fi
fi

AC_MSG_RESULT($WEBSTREAMDIR)
AC_SUBST(WEBSTREAMDIR)

])