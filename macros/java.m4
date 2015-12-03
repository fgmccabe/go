dnl NA_DETECT_JAVA(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl
dnl Description:
dnl   Detect the Java installation
dnl
dnl Defines 
dnl   JAVA_HOME - installation directory 
dnl   JAVAC     - the java compiler
dnl   JAVA      - the java virtual machine
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_JAVA,[

na_required=$1

AC_ARG_WITH(java,
[  --with-java[=dir]         indicate location of the java installation],
[na_java=${withval}],
[na_java=${JAVA_HOME}]
)

[na_parentprefix=`echo "$prefix"|sed -e 's%\(.\)/[^/]*/*$%\1%'`]

na_searchpath=${na_java}/bin:${PATH}:/opt/java/bin:$prefix/bin:$na_parentprefix/java/bin
AC_PATH_PROG(na_java,java,[not found],$na_searchpath)

if test "$na_java" = "not found" ; then
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([java not found in $na_searchpath
Run configure with --with-java to specify the Java
installation directory
])
  fi
fi

JAVA_HOME=`echo $na_java | sed -e s%/bin/java$%%` 
JAVAC=${JAVA_HOME}/bin/javac
JAVA=${JAVA_HOME}/bin/java

AM_CONDITIONAL(HAVE_JAVA, test -x "${JAVA}")

AC_SUBST(JAVA_HOME)
AC_SUBST(JAVAC)
AC_SUBST(JAVA)
])