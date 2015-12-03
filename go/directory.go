/*
   Directory service
   (c) 2004 F.G. McCabe
 
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

  Contact: Francis McCabe <mccabe@fla.fujitsu.com>
 */
 
directory{
  import go.io.
  import go.hash.

  directory = $hash([],32).

  desc[] ::=
          address(string)
        | att(symbol,string).

  description[] ::= entry(symbol,list[desc[]]).

  register(Owner,Desc) ->
      sync(directory){
        if directory.present(Owner,_) 
            raise error("duplicate",'fail')
       | _ -> 

  

 