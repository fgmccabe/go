/*
 * A basic synchronized object
 */

go.synchronized{
  synchronized[] <~ {}

  synchronized:synchronized[] .. {
    lock = __newLock().
  }
}.