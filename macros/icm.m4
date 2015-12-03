dnl NA_DETECT_ICM(REQUIRED)
dnl
dnl Arguments
dnl   REQUIRED - required/optional
dnl   VERSION  - an optional argument specifying
dnl              which version or later is required.
dnl
dnl Description:
dnl   Detect the ICM installation
dnl
dnl Defines 
dnl   ICMDIR - installation directory 
dnl
dnl Notes
dnl   If REQUIRED is set to 'required' and the package
dnl   is not found, a fatal error message will be issued
dnl   and configure will exit.

AC_DEFUN(NA_DETECT_ICM,[

na_required=$1
na_version_full=$2

AC_ARG_WITH(icm,
[  --with-icm[=dir]          indicate location of the ICM installation],
[na_icm=${withval}],
)

[na_parentprefix=`echo "$prefix"|sed -e 's%\(.\)/[^/]*/*$%\1%'`]

na_searchpath=${na_icm}/bin:${PATH}:/opt/icm/bin:$prefix/bin:$na_parentprefix/icm/bin
AC_PATH_PROG(na_icm,icm,[not found],$na_searchpath)

if test "$na_icm" = "not found" ; then
  if test "$na_required" = "required" ; then
    echo
    AC_MSG_ERROR([icm not found in $na_searchpath
Run configure with --with-icm to specify the ICM
installation directory
])
  fi
fi

ICMDIR=`echo $na_icm | sed -e s%/bin/icm$%%` 

# check the icm version, if specified
if test -n "$na_version_full" ; then
  [detected_version_full=`cat $ICMDIR/icm.spec | grep Version | sed -e 's/Version:[ 	]*//'`]

  detected_version=`echo "$detected_version_full" | sed -e 's/pre.*//'`
  case "$detected_version_full" in 
    *pre*) 
      detected_preversion=`echo "$detected_version_full" | sed -e 's/^.*pre//'` ;;
    *)
      detected_preversion=""
  esac

  na_version=`echo "$na_version_full" | sed -e 's/pre.*//'`
  case "$na_version_full" in 
    *pre*) 
      na_preversion=`echo "$na_version_full" | sed -e 's/^.*pre//'` ;;
    *)
      na_preversion=""
  esac

  na_version_failed=""
  if expr "$detected_version" \< "$na_version" > /dev/null ; then
     na_version_failed=yes
  elif expr "$detected_version" \= "$na_version" > /dev/null ; then
    case "$na_preversion" in 
      "") if test -n "$detected_preversion" ; then
            na_version_failed=yes
	  fi ;;
      *) case "$detected_preversion" in 
	   "") ;; # Not a preversion
	   *) if expr "$detected_preversion" \< "$na_preversion" > /dev/null ; then
	         na_version_failed=yes
	      fi ;;
	 esac ;;
    esac
  fi
  if test -z "$na_version_failed" ; then
    AC_MSG_RESULT([detected icm $detected_version_full (required $na_version_full)])
  else
     AC_MSG_ERROR([icm found, but is version $detected_version_full
which is less than the required version, $na_version_full
])
  fi
fi
AC_SUBST(ICMDIR)

])
