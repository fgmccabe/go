/* 
  This is where you define a new operator so that the compiler and
  the run-time system can see it
  (c) 2001 F.G.McCabe

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

/* Declare standard symbols and constructors */


infixOp(". ",1899,1900,1900,"statement separator")
infixOp("::=",1459,1460,1459,"user type definition")
infixOp("|",1249,1250,1250,"type union")
infixOp("?",1199,1200,1199,"conditional operator")
infixOp("=>",1199,1200,1199,"function arrow")
infixOp(":-",1199,1200,1199,"clause arrow")
infixOp(":--",1199,1200,1199,"strong clause")
infixOp("->",1199,1200,1199,"process rule")
infixOp("-->",1199,1200,1199,"grammar rule")
infixOp("*>",1151,1152,1151,"all solutions")
infixOp(";",1149,1150,1150,"action separator")
infixOp("::",1129,1125,1129,"guard marker")
infixOp("||",1059,1060,1059,"bag of constructor")
infixOp(",",999,1000,1000,"tupling, conjunction")
infixOp("onerror",954,955,954,"error handler")
infixOp("<=",949,950,949,"class rule arrow")
infixOp("<~",948,949,948,"implements interface")
infixOp("@",904,905,904,"tau pattern notation")
infixOp("@@",904,905,905,"suspension variable")
infixOp("timeout",899,900,899,"timeout clause")
infixOp("=",899,900,899,"variable declaration")
infixOp(":=",899,900,899,"variable assignment")
infixOp("==",899,900,899,"equality predicate")
infixOp("\\=",899,900,899,"not unifyable")
infixOp("!=",899,900,899,"not equal")
infixOp("<",899,900,899,"less than")
infixOp("=<",899,900,899,"less than or equal")
infixOp(">",899,900,899,"greater than")
infixOp(">=",899,900,899,"greater than or equal")
infixOp(".=",899,900,899,"match predicate")
infixOp("=.",899,900,899,"match predicate")
infixOp("..",895,896,895,"list abstraction")
infixOp("in",894,895,894,"set membership")
infixOp("\\/",820,820,819,"set union")
infixOp("\\",820,820,819,"set difference")
infixOp("/\\",800,800,799,"set intersection")
infixOp("<>",799,800,800,"list append")
infixOp("#",759,760,759,"package separator")
infixOp(":",749,750,749,"type annotation")
infixOp("$=",730,731,730,"constructor type")
infixOp("@>",730,731,730,"constructor type")
infixOp("@=",730,731,730,"constructor type")
infixOp("+",720,720,719,"addition")
infixOp("-",720,720,719,"subtraction")
infixOp("*",700,700,699,"multiplication")
infixOp("/",700,700,699,"division")
infixOp("quot",700,700,699,"integer quotient")
infixOp("rem",700,700,699,"remainder function")
infixOp("**",600,600,599,"exponentiation")
infixOp("%%",499,500,499,"grammar parse")
infixOp("^",499,500,499,"grammar iterator")
infixOp("~",934,935,934,"grammar remainder")
lastInfOp
infixOp(".",449,450,449,"object access")

prefixOp("private",1700,1699,"private program")
prefixOp("import",900,899,"import module")
prefixOp("case",950,949,"case analysis")
prefixOp("\\+",905,904,"logical negation")
prefixOp("@",905,904,"tau pattern")
prefixOp("raise",900,899,"raise exception")
prefixOp("valis",905,904,"return value")
prefixOp("istrue",905,904,"return value")
prefixOp("$",897,896,"initialization")
prefixOp(":",750,749,"type annotation")
lastPreOp
prefixOp("-",300,299,"arithmetic negation")

postfixOp(". ",1899,1900,"statement terminator")
postfixOp(";",1149,1150,"action terminator")
postfixOp("!",904,905,"one solution operator")
postfixOp("+",759,760,"input mode")
postfixOp("-",759,760,"output mode")
postfixOp("-+",759,760,"bidirectional mode")
postfixOp("+-",759,760,"bidirectional mode")
postfixOp("++",759,760,"super input mode")
postfixOp("*",699,700,"action type")
lastPostOp
postfixOp("^",499,500,"string convertion")

