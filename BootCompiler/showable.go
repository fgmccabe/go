/*
 * The showable interface
 */

org.go.sys.show{
  showable[] <~ { show:(()=>string) }.
}