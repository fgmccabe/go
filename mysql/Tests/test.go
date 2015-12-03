/*
 * Test the mysql interface
 */

main..{
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "sys:ext/mysql/include/libmysqlgo.gof".

  main() ->
      Q =__mysql_query("localhost","root","","test","select * from foo");
      N = __mysql_numrows(Q);
      F = __mysql_fetchfields(Q);
      stdout.outLine("Q="<>Q^0);
      stdout.outLine("N="<>N^0);
      stdout.outLine("F="<>F^0);
      fetchRows(Q).

  fetchRows(Q)::__mysql_fetchrow(Q,R) ->
      stdout.outLine("Row= "<>R^0);
      fetchRows(Q).
  fetchRows(_) -> {}.
}
      
        
