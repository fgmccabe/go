dnl NA_ENABLE_BUILDDOC()
dnl
dnl Arguments
dnl   None
dnl
dnl Description:
dnl   Provide --disable-builddoc option for configure
dnl   to prevent the building of documentation on the 
dnl   systems that do not have the appropriate tools.
dnl
dnl   Provide --paper-type option for specify letter
dnl   versus a4.
dnl
dnl Defines 
dnl   BUILDDOC - automake conditional
dnl

AC_DEFUN(NA_ENABLE_BUILDDOC,[

AC_ARG_ENABLE(builddoc,
     [  --enable-builddoc       build documentation (default is to build)],
     [case "${enableval}" in
       yes) builddoc=true ;;
       no)  builddoc=false ;;
       *) AC_MSG_ERROR(bad value ${enableval} for --enable-builddoc) ;;
     esac] ,[builddoc=true])

AC_ARG_WITH(paper,
     [  --with-paper=a4         specify documentation paper type (default is letter)],
     [case "${withval}" in 
       a4|A4) PAPERTYPE=a4 ;;
       letter|LETTER|Letter) PAPERTYPE=letter ;;
       *) AC_MSG_ERROR(bad value ${papertype} for --with-paper-type (allowable values are: a4 letter))
      esac], [PAPERTYPE=letter])

AC_SUBST(PAPERTYPE)
     
AM_CONDITIONAL(BUILDDOC, test x$builddoc = xtrue)
])
