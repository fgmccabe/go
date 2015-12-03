/*
  List view of I/O
  Allows file input to a grammar parser
  (c) 2006 F.G. McCabe
 
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

  Contact: Francis McCabe <fgm@fla.fujitsu.com>
 */

go.stream{
  import go.io.

  inputSequence:[inChannel,integer]@=list[char].
  inputSequence(F,filePosition)..{

    eof():-
	action{adjustOffset()},
	F.eof().

    head()::F.eof() => raise error("end of stream",'eFAIL').
    head()=>valof{
	      adjustOffset();
	      valis F.inCh();
	    }.

    cons(H) => [H,..this].

    tack(_) => raise error("tack not supported",'eINVAL').

    eq(this).

    hdtl(H,T) :-
	action{
	  adjustOffset()
	},
	\+F.eof(),
	H = F.inCh(),
	T = tail().

    tail()=>inputSequence(F,filePosition+1).

    private adjustOffset:[]*.
    adjustOffset()->
	(F.pos()!=filePosition ?
	   F.seek(filePosition)).
  }.

  listFile:[string,ioEncoding]=>list[char].
  listFile(Fl,Enc) => inputSequence(openInFile(Fl,Enc),0).

  listSocket:[string,integer,ioEncoding]=>(list[char],outChannel).
  listSocket(Host,Port,Enc) => valof{
				 tcpConnect(Host,Port,inChnl,outChnl,Enc);
				 valis (inputSequence(inChnl,0),outChnl)
			       }.
}