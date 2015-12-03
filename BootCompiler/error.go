/*
 * The base exception class module
 */

error{
  syserror[] <~ { cause:(()=>string), code:(()=>symbol) }.

  error(Cause,Code):syserror[]..{
    cause()=>Cause.
    code()=>Code.
  }
}