/* Parser generated automatically from "ul"*/
ul{


import ulex.

yyType ::= 
   N(integer)		-- number
 | I(string)		-- identifier
 | SEQ(yyType,yyType)		-- sequence of statements
 | EXIT			-- exit statement
 | PR(yyType)		-- print statement
 | ASS(string,yyType)	-- assignment statement
 | PL(yyType,yyType)		-- expressions
 | MI(yyType,yyType)
 | ST(yyType,yyType)
 | DV(yyType,yyType)
 | UM(yyType) .


  private __item::= __t(yyToken) | __n(yyType).

  failed:[]@=exception.
  failed..{
    cause() => "failed".
    code() => 'failed'.
    show() => "failed".
   }.

  doParse <~ { parse:[integer,yyToken,list[integer],list[__item]]=>yyType. }.
  parser:[yyLexer]=>yyType.
  parser(lexer) => 
     (:doParse..{
    yyErrCount:integer := 0.

    parse(Sno,Tok,Stack,Vstack) => do(Sno,Tok,Stack,Vstack) onerror (
      failed => valof{
         yyErrCount := yyErrCount+1;
         raise error("unrecoverable parse error",'parse');
         valis _
      }).
    do:[integer,yyToken,list[integer],list[__item]]=>yyType.
    do(31,Tok,Stack,Vstack) => valof{
      [_,__n(__V3),_,__t(__T1@isToken(ID(__V1))),..__Vstack] .= Vstack;
      valis do(gotoassignment(drop(Stack,3).head()),Tok,drop(Stack,3),[__n( ASS(__V1,__V3) ),..__Vstack])
    }.
    do(30,Tok,Stack,Vstack) =>
      case Tok.token() in (
        PLUS =>         do(20,lexer.nextToken(),[30,..Stack],[__t(Tok),..Vstack])
      | MINUS =>         do(21,lexer.nextToken(),[30,..Stack],[__t(Tok),..Vstack])
      | STAR =>         do(22,lexer.nextToken(),[30,..Stack],[__t(Tok),..Vstack])
      | SLASH =>         do(23,lexer.nextToken(),[30,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V3),_,__n(__V1),..__Vstack] .= Vstack;
      valis do(gotoexp(drop(Stack,2).head()),Tok,drop(Stack,2),[__n( PL(__V1,__V3) ),..__Vstack])
    }
    ).
    do(29,Tok,Stack,Vstack) =>
      case Tok.token() in (
        PLUS =>         do(20,lexer.nextToken(),[29,..Stack],[__t(Tok),..Vstack])
      | MINUS =>         do(21,lexer.nextToken(),[29,..Stack],[__t(Tok),..Vstack])
      | STAR =>         do(22,lexer.nextToken(),[29,..Stack],[__t(Tok),..Vstack])
      | SLASH =>         do(23,lexer.nextToken(),[29,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V3),_,__n(__V1),..__Vstack] .= Vstack;
      valis do(gotoexp(drop(Stack,2).head()),Tok,drop(Stack,2),[__n( MI(__V1,__V3) ),..__Vstack])
    }
    ).
    do(28,Tok,Stack,Vstack) =>
      case Tok.token() in (
        PLUS =>         do(20,lexer.nextToken(),[28,..Stack],[__t(Tok),..Vstack])
      | MINUS =>         do(21,lexer.nextToken(),[28,..Stack],[__t(Tok),..Vstack])
      | STAR =>         do(22,lexer.nextToken(),[28,..Stack],[__t(Tok),..Vstack])
      | SLASH =>         do(23,lexer.nextToken(),[28,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V3),_,__n(__V1),..__Vstack] .= Vstack;
      valis do(gotoexp(drop(Stack,2).head()),Tok,drop(Stack,2),[__n( ST(__V1,__V3) ),..__Vstack])
    }
    ).
    do(27,Tok,Stack,Vstack) =>
      case Tok.token() in (
        PLUS =>         do(20,lexer.nextToken(),[27,..Stack],[__t(Tok),..Vstack])
      | MINUS =>         do(21,lexer.nextToken(),[27,..Stack],[__t(Tok),..Vstack])
      | STAR =>         do(22,lexer.nextToken(),[27,..Stack],[__t(Tok),..Vstack])
      | SLASH =>         do(23,lexer.nextToken(),[27,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V3),_,__n(__V1),..__Vstack] .= Vstack;
      valis do(gotoexp(drop(Stack,2).head()),Tok,drop(Stack,2),[__n( DV(__V1,__V3) ),..__Vstack])
    }
    ).
    do(26,Tok,Stack,Vstack) => valof{
      [_,__n(__V2),_,..__Vstack] .= Vstack;
      valis do(gotoexp(drop(Stack,2).head()),Tok,drop(Stack,2),[__n( __V2 ),..__Vstack])
    }.
    do(25,Tok,Stack,Vstack) => 
       case Tok.token() in (
        RPAR => 
          do(26,lexer.nextToken(),[25,..Stack],[__t(Tok),..Vstack])
      | PLUS => 
          do(20,lexer.nextToken(),[25,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(21,lexer.nextToken(),[25,..Stack],[__t(Tok),..Vstack])
      | STAR => 
          do(22,lexer.nextToken(),[25,..Stack],[__t(Tok),..Vstack])
      | SLASH => 
          do(23,lexer.nextToken(),[25,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(24,Tok,Stack,Vstack) =>
      case Tok.token() in (
        PLUS =>         do(20,lexer.nextToken(),[24,..Stack],[__t(Tok),..Vstack])
      | MINUS =>         do(21,lexer.nextToken(),[24,..Stack],[__t(Tok),..Vstack])
      | STAR =>         do(22,lexer.nextToken(),[24,..Stack],[__t(Tok),..Vstack])
      | SLASH =>         do(23,lexer.nextToken(),[24,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V2),_,..__Vstack] .= Vstack;
      valis do(gotoexp(drop(Stack,1).head()),Tok,drop(Stack,1),[__n( UM(__V2) ),..__Vstack])
    }
    ).
    do(23,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(16,lexer.nextToken(),[23,..Stack],[__t(Tok),..Vstack])
      | INT(_) => 
          do(15,lexer.nextToken(),[23,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(13,lexer.nextToken(),[23,..Stack],[__t(Tok),..Vstack])
      | LPAR => 
          do(12,lexer.nextToken(),[23,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(22,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(16,lexer.nextToken(),[22,..Stack],[__t(Tok),..Vstack])
      | INT(_) => 
          do(15,lexer.nextToken(),[22,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(13,lexer.nextToken(),[22,..Stack],[__t(Tok),..Vstack])
      | LPAR => 
          do(12,lexer.nextToken(),[22,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(21,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(16,lexer.nextToken(),[21,..Stack],[__t(Tok),..Vstack])
      | INT(_) => 
          do(15,lexer.nextToken(),[21,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(13,lexer.nextToken(),[21,..Stack],[__t(Tok),..Vstack])
      | LPAR => 
          do(12,lexer.nextToken(),[21,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(20,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(16,lexer.nextToken(),[20,..Stack],[__t(Tok),..Vstack])
      | INT(_) => 
          do(15,lexer.nextToken(),[20,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(13,lexer.nextToken(),[20,..Stack],[__t(Tok),..Vstack])
      | LPAR => 
          do(12,lexer.nextToken(),[20,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(19,Tok,Stack,Vstack) => valof{
      [_,__n(__V2),_,..__Vstack] .= Vstack;
      valis do(gotoprint(drop(Stack,2).head()),Tok,drop(Stack,2),[__n( PR(__V2) ),..__Vstack])
    }.
    do(18,Tok,Stack,Vstack) => 
       case Tok.token() in (
        SEMI => 
          do(31,lexer.nextToken(),[18,..Stack],[__t(Tok),..Vstack])
      | PLUS => 
          do(20,lexer.nextToken(),[18,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(21,lexer.nextToken(),[18,..Stack],[__t(Tok),..Vstack])
      | STAR => 
          do(22,lexer.nextToken(),[18,..Stack],[__t(Tok),..Vstack])
      | SLASH => 
          do(23,lexer.nextToken(),[18,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(17,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(16,lexer.nextToken(),[17,..Stack],[__t(Tok),..Vstack])
      | INT(_) => 
          do(15,lexer.nextToken(),[17,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(13,lexer.nextToken(),[17,..Stack],[__t(Tok),..Vstack])
      | LPAR => 
          do(12,lexer.nextToken(),[17,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(16,Tok,Stack,Vstack) => valof{
      [__t(__T1@isToken(ID(__V1))),..__Vstack] .= Vstack;
      valis do(gotoexp(Stack.head()),Tok,Stack,[__n( I(__V1) ),..__Vstack])
    }.
    do(15,Tok,Stack,Vstack) => valof{
      [__t(__T1@isToken(INT(__V1))),..__Vstack] .= Vstack;
      valis do(gotoexp(Stack.head()),Tok,Stack,[__n( N(__V1) ),..__Vstack])
    }.
    do(14,Tok,Stack,Vstack) => 
       case Tok.token() in (
        SEMI => 
          do(19,lexer.nextToken(),[14,..Stack],[__t(Tok),..Vstack])
      | PLUS => 
          do(20,lexer.nextToken(),[14,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(21,lexer.nextToken(),[14,..Stack],[__t(Tok),..Vstack])
      | STAR => 
          do(22,lexer.nextToken(),[14,..Stack],[__t(Tok),..Vstack])
      | SLASH => 
          do(23,lexer.nextToken(),[14,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(13,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(16,lexer.nextToken(),[13,..Stack],[__t(Tok),..Vstack])
      | INT(_) => 
          do(15,lexer.nextToken(),[13,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(13,lexer.nextToken(),[13,..Stack],[__t(Tok),..Vstack])
      | LPAR => 
          do(12,lexer.nextToken(),[13,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(12,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(16,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | INT(_) => 
          do(15,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(13,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | LPAR => 
          do(12,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(11,Tok,Stack,Vstack) => valof{
      [_,_,..__Vstack] .= Vstack;
      valis do(gotohalt(drop(Stack,1).head()),Tok,drop(Stack,1),[__n( EXIT ),..__Vstack])
    }.
    do(10,Tok,Stack,Vstack) => valof{
      [__n(__V2),__n(__V1),..__Vstack] .= Vstack;
      valis do(gotostatements(drop(Stack,1).head()),Tok,drop(Stack,1),[__n( SEQ(__V1,__V2) ),..__Vstack])
    }.
    do(9,Tok,Stack,Vstack) =>
      case Tok.token() in (
        EOF => ( yyErrCount>0 ?
                 raise error(yyErrCount.show()<>" parse errors detected",'eFAIL')
               | valof{
                 [__n(__Vx),.._] .= Vstack;
                 valis __Vx;
               }
               )

    ).
    do(8,Tok,Stack,Vstack) =>
      case Tok.token() in (
        ID(_) =>         do(1,lexer.nextToken(),[8,..Stack],[__t(Tok),..Vstack])
      | PRINT =>         do(2,lexer.nextToken(),[8,..Stack],[__t(Tok),..Vstack])
      | HALT =>         do(3,lexer.nextToken(),[8,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotoprogram(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }
    ).
    do(7,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotostatements(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(6,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotostatement(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(5,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotostatement(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(4,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotostatement(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(3,Tok,Stack,Vstack) => 
       case Tok.token() in (
        SEMI => 
          do(11,lexer.nextToken(),[3,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(2,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(16,lexer.nextToken(),[2,..Stack],[__t(Tok),..Vstack])
      | INT(_) => 
          do(15,lexer.nextToken(),[2,..Stack],[__t(Tok),..Vstack])
      | MINUS => 
          do(13,lexer.nextToken(),[2,..Stack],[__t(Tok),..Vstack])
      | LPAR => 
          do(12,lexer.nextToken(),[2,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(1,Tok,Stack,Vstack) => 
       case Tok.token() in (
        EQUAL => 
          do(17,lexer.nextToken(),[1,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(0,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(1,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | PRINT => 
          do(2,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | HALT => 
          do(3,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).

  scanForToken:[yyTokType]*.
  scanForToken(Tok)->
     nxT = lexer.nextToken();
     ( nxT.token()\=Tok ? scanForToken(Tok)).

  }).parse(0,lexer.nextToken(),[],[]).

gotoexp:[integer]=>integer.
gotoexp(20) => 30.
gotoexp(21) => 29.
gotoexp(22) => 28.
gotoexp(23) => 27.
gotoexp(12) => 25.
gotoexp(13) => 24.
gotoexp(17) => 18.
gotoexp(2) => 14.

gotohalt:[integer]=>integer.
gotohalt(8) => 4.
gotohalt(0) => 4.

gotoprint:[integer]=>integer.
gotoprint(8) => 5.
gotoprint(0) => 5.

gotoassignment:[integer]=>integer.
gotoassignment(8) => 6.
gotoassignment(0) => 6.

gotostatement:[integer]=>integer.
gotostatement(8) => 10.
gotostatement(0) => 7.

gotostatements:[integer]=>integer.
gotostatements(0) => 8.

gotoprogram:[integer]=>integer.
gotoprogram(0) => 9.

}

