dnl NA_APRIL_EXT
dnl
dnl Arguments
dnl    None
dnl
dnl Description
dnl    Checks for typedefs, structures, and compiler characteristics
dnl    required for compiling April Extensions.
dnl
dnl Notes
dnl    None
dnl

AC_DEFUN(NA_APRIL_EXT,[

AC_C_CONST
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM

AC_MSG_CHECKING([for alignment of long long])
AC_CACHE_VAL(april_llong_alignment,
[AC_TRY_RUN([
int main(){
  long v1;
  long long v2;
  struct {
    long v1;
    long long v2;
  } v3;

  if(sizeof(v3)==sizeof(v1)+sizeof(v2))
    exit(0);
  else
    exit(1);
}],
april_llong_alignment=align32,
april_llong_alignment=align64,
april_llong_alignment=align64)])

if test $april_llong_alignment = align64; then
  AC_DEFINE(LLONG_ALIGNMENT,1,[Define with 1 if long long alignment is necessary])
else
  AC_DEFINE(LLONG_ALIGNMENT,0,[Define with 1 if long long alignment is necessary])
fi
AC_MSG_RESULT($april_llong_alignment)
 
AC_MSG_CHECKING([for alignment of double])
AC_CACHE_VAL(april_double_alignment,
[AC_TRY_RUN([
int main(){
  long v1;
  double v2;
  struct {
    long v1;
    double v2;
  } v3;

  if(sizeof(v3)==sizeof(v1)+sizeof(v2))
    exit(0);
  else
    exit(1);
}],
april_double_alignment=align32,
april_double_alignment=align64,
april_double_alignment=align64)])

if test $april_double_alignment = align64; then
  AC_DEFINE(DOUBLE_ALIGNMENT,1,[Define with 1 if double alignment is necessary])
else
  AC_DEFINE(DOUBLE_ALIGNMENT,0,[Define with 1 if double alignment is necessary])
fi
AC_MSG_RESULT($april_double_alignment)

AC_CACHE_VAL(april_word32,
[
AC_CHECK_SIZEOF(long,4)
AC_CHECK_SIZEOF(int,4)
AC_CHECK_SIZEOF(short int,2)

if test $ac_cv_sizeof_int = 4; then
  april_word32="int"
elif test $ac_cv_sizeof_short_int = 4; then
  april_word32="short int"
elif test $ac_cv_sizeof_long = 4; then
  april_word32="long"
else
  AC_MSG_ERROR([cant determine a 32 bit int type])
fi
])
AC_DEFINE_UNQUOTED(WORD32,$april_word32,[Define with a type that is 32 bits long])

AC_MSG_CHECKING([32 bit int type])
AC_MSG_RESULT($april_word32)
 
AC_CACHE_VAL(april_word64,
[
AC_CHECK_SIZEOF(long long,8)
AC_CHECK_SIZEOF(long,4)

if test $ac_cv_sizeof_long_long = 8; then
  april_word64="long long"
elif test $ac_cv_sizeof_long = 8; then
  april_word64="long"
else
  AC_MSG_ERROR([cant determine a 64 bit int type])
fi
])
AC_DEFINE_UNQUOTED(WORD64,$april_word64,[Define with a type that is 64 bits long])

AC_MSG_CHECKING([64 bit int type])
AC_MSG_RESULT($april_word64)

AC_MSG_CHECKING([if compiler allows zero array size])
save_cflags="$CFLAGS"
CFLAGS="$CFLAGS -Wall"
AC_TRY_COMPILE([],
[
struct {
  int one;
  char somemore[];
};
],
AC_DEFINE(ZEROARRAYSIZE,,[Allow empty arrays])
AC_MSG_RESULT(no),
AC_DEFINE(ZEROARRAYSIZE,0,[Don't Allow empty arrays])
AC_MSG_RESULT(yes))
CFLAGS="$save_cflags"

])
