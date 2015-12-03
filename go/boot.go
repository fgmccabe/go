/*
  Bootstrap program, this program is loaded automatically 
  when the Go! engine is started. Its main function is to set up the 
  environment and then load and execute the program mentioned in the command line
  
  (c) 2001-2005 F.G. McCabe

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
go.boot{
  __main:[string,list[(char,symbol)],list[string]]*.
  __main(Path,Options,Cmd) -> 
      ([Prog,..Args] = Cmd ?
	 pkg = implode(chopGoc(Prog));
	 entry = __default(`m,Options,'main');
	 __ensure_loaded(Path,pkg,__default(`M,Options,''),depends)
     | raise error("missing package name",'eNOTFND')
      )
      onerror(
       Err -> {__logmsg("Fatal error on load: "<>Err.show())}; exit(1)
      );
      execInits(reverse(depends));
      (__defined(pkg,entry,1)?
         ((__call(pkg,entry,1,Args);exit(0)) onerror ( -- __call may *not* be the last call!!!!
        E -> {__logmsg("Fatal error("<>E.show()<>")")};exit(1)
          ))
     | {__logmsg("Entry: "<>explode(entry)<>" not defined in "<>Prog)};exit(1)
      ).
  
  private execInits:[list[symbol]]*.
  execInits([]) -> {}.
  execInits([P,..L]) -> 
      __call(P,'$init',0,[]);
      execInits(L).

  private __default:[char,list[(char,symbol)],symbol]=>symbol.
  __default(C,Opts,_)::(C,Val) in Opts => Val.
  __default(_,_,Def) => Def.

  private chopGoc:[string]=>string.
  chopGoc([])=>[].
  chopGoc(".goc")=>[].
  chopGoc([C,..L])=>[C,..chopGoc(L)].
}
