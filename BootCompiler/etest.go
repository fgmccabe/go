/*
 * Test the error handling code
 */

etest{
  check(X) => (divZero(X) onerror ( error("foo",'fail') => X | error(_,_) => -X))*2.

  divZero(X) => X/0.
}
