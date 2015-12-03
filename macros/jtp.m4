dnl NA_DETECT_JTP(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl
dnl Description:
dnl   Detect the JTP installation
dnl
dnl Defines 
dnl   JTP_DAML_HOME - The location of the installed JTP
dnl   JTP_CLASSPATH - a class path to link against
dnl   HAVE_JTP      - only if jtp was successfully detected
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_JTP,[

na_required=$1

AC_ARG_WITH(jtp,
[  --with-jtp[=dir]          indicate location of the JTP installation],
[na_jtp=${withval}],
[na_jtp="no"]
)

if test "$na_jtp" = "no" ; then
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([jtp_config not found in $na_searchpath
Run configure with --with-jtp to specify the JTP
installation directory
])
  fi
fi

AM_CONDITIONAL(HAVE_JTP, test "$na_jtp" != "no")

JTP_DAML_HOME=$na_jtp
JTP_CLASSPATH=""

if test "${JTP_DAML_HOME}" != "no" ; then
if test -d ${JTP_DAML_HOME} ; then 

# Pick up all the jars in JTP DAML lib dist
for i in `ls ${JTP_DAML_HOME}/lib/*.jar` ; do
    if test "$JTP_CLASSPATH" != "" ; then
	JTP_CLASSPATH=${JTP_CLASSPATH}:$i
    else
	JTP_CLASSPATH=$i
    fi
done

else
AC_MSG_ERROR([Could not access directory $JTP_DAML_HOME])
fi
fi

AC_SUBST(JTP_DAML_HOME)
AC_SUBST(JTP_CLASSPATH)

])