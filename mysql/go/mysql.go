/*
 * A library module to give higher-level access to the mysql database
 * functions
 */

(mysql)..{
  include "sys:go/stdlib.gof".
  include "sys:ext/mysql/libmysqlgo.gof".

  mysql(Host,User,Pass,Db){
    mem(Table,P) :- Candidates = valof{
                      sync{
                        Q = __mysql_query(Host,User,Pass,Db,
                                          "select * from "<>Table);
                        valis _collectResults(Q)
                      }
                    },
                    P in Candidates.

    query(Query,P) :- Candidates = valof{
                        sync{
                          Q = __mysql_query(Host,User,Pass,Db,Query);
                          valis _collectResults(Q)
                        }
                      },
                      P in Candidates.

    _collectResults(Q) => [Entry,.._collectResults(Q)] :-
        __mysql_fetchrow(Q,Entry).
    _collectResults(Q) => [] :- __mysql_freeresult(Q).
  }.
}.
