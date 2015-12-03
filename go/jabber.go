jabber{
 /*
  * A package to permit a go agent to send and receive messages as a 
  * Jabber client.
  * (c) 2006 F. G. McCabe (frankmccabe@mac.com)
  * This program is free software; you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or
  * (at your option) any later version.

  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.

  * You should have received a copy of the GNU General Public License
  * along with this program; if not, write to the Free Software
  * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  */
  import go.io.
  import go.xml.

  jabberConnect:[string,integer,string,string]*.
  jabberConnect(Host,Port,ServerName,User) ->
      tcpConnect(Host,port,inCh,outCh,utf8Encoding);
      outCh.outLine("<stream:stream to='"<>ServerName<>"' from='"<>User<>
		    "'xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>");
      waitForStream(inCh).

  private waitForStream:[inChannel]*.
  waitForStream(inCh) -> {}.
      
      


  

 
 
}