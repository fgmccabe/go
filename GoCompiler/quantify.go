/*
   Requantify a program after type checking.
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
quantify{
  import opts.
  import types.
  import terms.

  reQuantify:[progTree,list[vEntry]]=>progTree.
  reQuantify(srDf(Name,Vis,Rules,Tp,Lc),Env) =>
      srDf(Name,Vis,{reQuantRule(Rule,Env) .. Rule in Rules},Tp,Lc).
  reQuantify(rDf(Name,Vis,Rules,Tp,Lc),Env) =>
      rDf(Name,Vis,{reQuantRule(Rule,Env) .. Rule in Rules},Tp,Lc).
  reQuantify(fnDf(Name,Vis,Rules,Tp,Lc),Env) =>
      fnDf(Name,Vis,{reQuantRule(Rule,Env) .. Rule in Rules},Tp,Lc).
  reQuantify(grDf(Name,Vis,Rules,Tp,Lc),Env) =>
      grDf(Name,Vis,{reQuantRule(Rule,Env) .. Rule in Rules},Tp,Lc).
  reQuantify(acDf(Name,Vis,Rules,Tp,Lc),Env) =>
      acDf(Name,Vis,{reQuantRule(Rule,Env) .. Rule in Rules},Tp,Lc).
  reQuantify(clDf(Name,Vis,Rules,Tp,Lc),Env) =>
      clDf(Name,Vis,{reQuantRule(Rule,Env) .. Rule in Rules},Tp,Lc).

  private reQuantRule:[ruleTree,list[vEntry]]=>ruleTree.
  reQuantRule(cls(Q,Args,Body,Lc),Env) =>
      valof{
	xQ = varsIn
		 
}