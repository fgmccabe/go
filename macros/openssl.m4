dnl NA_DETECT_OPENSSL(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl
dnl Description:
dnl   Detect the OpenSSL installation
dnl
dnl Defines 
dnl   OPENSSLDIR - openssl installation prefix
dnl   OPENSSLBIN - path to openssl binary
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_OPENSSL,[

na_required=$1

AC_ARG_WITH(openssl,
[  --with-openssl[=dir]      indicate location of the openssl installation],
[na_openssl=${withval}],
[na_openssl=${OPENSSLDIR}]
)

[na_parentprefix=`echo "$prefix"|sed -e 's%\(.\)/[^/]*/*$%\1%'`]

na_searchpath=${na_openssl}/bin:${PATH}:/usr/bin:/usr/local/bin:/opt/openssl/bin:$prefix/bin:$na_parentprefix/openssl/bin
AC_PATH_PROG(na_openssl,openssl,[not found],$na_searchpath)

if test "$na_openssl" = "not found" ; then
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([openssl not found in $na_searchpath
Run configure with --with-openssl to specify the Openssl
installation directory
])
  fi
fi

OPENSSLDIR=`echo $na_openssl | sed -e s%/bin/openssl$%%` 
AC_SUBST(OPENSSLDIR)

OPENSSLBIN=$na_openssl
AC_SUBST(OPENSSLBIN)

])