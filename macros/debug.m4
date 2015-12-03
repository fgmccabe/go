dnl NA_DEBUG(default)
dnl
dnl Description:
dnl   Enable debugging
dnl
dnl Defines 
dnl   ALLTRACE
dnl

AC_DEFUN(NA_DEBUG,[

na_debug=$1

dnl Set up debugging flag
AC_ARG_ENABLE(debug,
[  --enable-debug          Turn on debugging code],
[case "${enableval}" in
  yes)
	CFLAGS='-g -Wall'
	AC_DEFINE(ALLTRACE)
	;;
  semi)
        CFLAGS='-g -Wall -O2'
        AC_DEFINE(ALLTRACE)
        ;;
  profile)
	CFLAGS='-g -pg -Wall'
	AC_DEFINE(ALLTRACE)
	;;

  no)  	CFLAGS='-O3 -Wall -DNDEBUG'
       	AC_DEFINE(NDEBUG)
       	AC_DEFINE(NOCOLOURS)
      	;;
  *)    CFLAGS='${enableval}'
        AC_DEFINE(ALLTRACE)
	;;
esac],[CFLAGS='-O3 -Wall'
AC_DEFINE(NDEBUG)
AC_DEFINE(NOCOLOURS)])

])