/*
 Package to manage importing of other packages

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

  Contact: Francis McCabe <frankmccabe@mac.com>
 */
imports{
  import go.io.
  import terms.
  import types.
  import decode.
  import opts.
  import misc.

  importPkg:[typeOpts,symbol]=> (symbol,symbol,dict,dict).
  importPkg(Opts,Pkg) =>
      valof{
	( Opts.option(classPath(Dirs)),
	  findFile(Dirs,expand(explode(Pkg),"."),Fl) ?
	    Nme = explode(decodeSymbol(Fl));
	    Vers = decodeSymbol(Fl);
	    Tps = fieldsOf(decodeType(Fl));
            Extra = fieldsOf(decodeType(Fl));
            Fl.close();
            valis (implode(Nme),(Vers='*'?''|Vers),Tps,Extra)
        | raise error("cannot find package "<>explode(Pkg),'fail')
	)
      }.

  private findFile:[list[string],list[string],inChannel-]{}.
  findFile([D,.._],Pkg,Fl) :-
      xFn = D<>collapse(Pkg,"/")<>".goc",
      ffile(xFn),
      Fl =openInFile(xFn,rawEncoding).
  findFile([D,.._],Pkg,Fl) :-
      yFn = trimDirs(D,Pkg)<>collapse(Pkg,"/")<>".goc",
      ffile(yFn),
      Fl = openInFile(yFn,rawEncoding).
  findFile([_,..Dirs],Pkg,Fl) :-
      findFile(Dirs,Pkg,Fl).

  private trimDirs:[string,list[string]]=>string.
  trimDirs(D,[Pre,..Rest])::ends(D,Pre) => trimDirs(trimD(D,Pre),Rest).
  trimDirs(D,_) => D.

  private ends:[string,string]{}.
  ends(S1,S2) :- append(_,[`/,..M],S1), append(S2,"/",M).

  private trimD:[string,string]=>string.
  trimD(S1,S2)::append(M,"/",S1),append(F,S2,M) => F.
}