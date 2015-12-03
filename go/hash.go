/*
  A hashtable class, supports a more efficient lookup than standard dynamic relations
  (c) 2001-2006 F.G. McCabe

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>
*/

go.hash{
  import go.io.

  hash[Ky,Vl] <~ { 
	insert:[Ky,Vl]*. 
	find:[Ky]=>Vl. 
	present:[Ky+,Vl]{}. 
	delete:[Ky]*.
	ext:[]=>list[(Ky,Vl)]. 
	keys:[]=>list[Ky]. 
	values:[]=>list[Vl]. 
	count:[]=>integer
      }.

  hash:[list[(Ktp,Vtp)],integer]@>hash[Ktp,Vtp].
  hash(I,Size)..{
    table:opaque := __newhash(max(Size,listlen(I)*2)).

    ${
      ((H,V) in I *> table:=__hashinsert(table,H,V));
    }.
      
    insert(Ky,Vl) -> sync{ table:=__hashinsert(table,Ky,Vl)}
        onerror(
         error(_,Code) ->
             raise error("insert",Code)
        ).
    
    find(Ky) => valof{
                  sync{
                    __hashsearch(table,Ky,Value) ?
                      valis Value
                   | raise error("find",'eNOTFND')
                  }
                }.
    
    present(Ky,Value) :-
        action{ sync{ __hashsearch(table,Ky,Value) ? valis true | valis false}}.
    
    count() => valof{
      sync{
        valis __hashcount(table);
      }
    }.
    
    delete(Ky) -> sync{ 
          __hashdelete(table,Ky)
      } onerror(
        error(_,Code) ->
         raise error("delete",Code)
      ).
    
    ext() => valof{
      sync{
        valis __hashcontents(table)
      } onerror(
        error(_,Code) ->
         raise error("ext",Code)
      )
    }.

    keys() => valof{
      sync{
        valis __hashkeys(table)
      } onerror(
        error(_,Code) ->
         raise error("keys",Code)
      )
    }.

    values() => valof{
      sync{
        valis __hashvalues(table)
      } onerror(
        error(_,Code) ->
         raise error("ext",Code)
      )
    }.
  }.

}
