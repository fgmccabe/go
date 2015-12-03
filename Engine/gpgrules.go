/* Parser generated automatically from "gpg.gpgrules"*/
gpg.gpgrules{
import gpg.gpglex.
import gpg.abstract.

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
         valis popErrorStates(Sno,Tok,Stack,Vstack);
      }).
    do:[integer,yyToken,list[integer],list[__item]]=>yyType.
    do(51,Tok,Stack,Vstack) => valof{
      [__n(__V4),__t(__T3@isToken(ID(__V3))),_,__n(__V1),..__Vstack] .= Vstack;
      valis do(gotoproduction(drop(Stack,3).head()),Tok,drop(Stack,3),[__n(RULE(__V1,__V4,I(__V3))),..__Vstack])
    }.
    do(50,Tok,Stack,Vstack) => valof{
      [__n(__V3),_,__n(__V1),..__Vstack] .= Vstack;
      valis do(gotoruleset(drop(Stack,2).head()),Tok,drop(Stack,2),[__n(CHOICE(__V1,__V3)),..__Vstack])
    }.
    do(49,Tok,Stack,Vstack) =>
      case Tok.token() in (
        BAR =>         do(42,lexer.nextToken(),[49,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V3),_,_,..__Vstack] .= Vstack;
      valis do(gotoruleset(drop(Stack,2).head()),Tok,drop(Stack,2),[__n(__V3),..__Vstack])
    }
    ).
    do(48,Tok,Stack,Vstack) =>
      case Tok.token() in (
        BRACE(_) =>         do(44,lexer.nextToken(),[48,..Stack],[__t(Tok),..Vstack])
      | _ => do(gotoaction(48),Tok,[48,..Stack],[__n(EMPTY),..Vstack])
    ).
    do(47,Tok,Stack,Vstack) => valof{
      [__t(__T2@isToken(ID(__V2))),__n(__V1),..__Vstack] .= Vstack;
      valis do(gotorhs(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(SEQ(__V1,I(__V2))),..__Vstack])
    }.
    do(46,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(48,lexer.nextToken(),[46,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(45,Tok,Stack,Vstack) => valof{
      [__n(__V2),__n(__V1),..__Vstack] .= Vstack;
      valis do(gotoproduction(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(RULE(__V1,__V2,EMPTY)),..__Vstack])
    }.
    do(44,Tok,Stack,Vstack) => valof{
      [__t(__T1@isToken(BRACE(__V1))),..__Vstack] .= Vstack;
      valis do(gotoaction(Stack.head()),Tok,Stack,[__n(I(__V1)),..__Vstack])
    }.
    do(43,Tok,Stack,Vstack) =>
      case Tok.token() in (
        ID(_) =>         do(37,lexer.nextToken(),[43,..Stack],[__t(Tok),..Vstack])
      | ID(_) =>         do(37,lexer.nextToken(),[43,..Stack],[__t(Tok),..Vstack])
      | BRACE(_) => do(gotorhs(43),Tok,[43,..Stack],[__n(EMPTY),..Vstack])
      | SEMI => do(gotorhs(43),Tok,[43,..Stack],[__n(EMPTY),..Vstack])
      | PREC => do(gotorhs(43),Tok,[43,..Stack],[__n(EMPTY),..Vstack])
      | _ => valof{
        scanForToken(BAR);
        valis do(38,lexer.currentToken(),[43,..Stack],Vstack)
      }

    ).
    do(42,Tok,Stack,Vstack) =>
      case Tok.token() in (
        ID(_) =>         do(37,lexer.nextToken(),[42,..Stack],[__t(Tok),..Vstack])
      | ID(_) =>         do(37,lexer.nextToken(),[42,..Stack],[__t(Tok),..Vstack])
      | _ => do(gotorhs(42),Tok,[42,..Stack],[__n(EMPTY),..Vstack])
    ).
    do(41,Tok,Stack,Vstack) => valof{
      [_,__n(__V3),_,__t(__T1@isToken(ID(__V1))),..__Vstack] .= Vstack;
      valis do(gotorule(drop(Stack,3).head()),Tok,drop(Stack,3),[__n(RULESET(__V1,__V3)),..__Vstack])
    }.
    do(40,Tok,Stack,Vstack) => 
       case Tok.token() in (
        SEMI => 
          do(41,lexer.nextToken(),[40,..Stack],[__t(Tok),..Vstack])
      | BAR => 
          do(42,lexer.nextToken(),[40,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(39,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotoruleset(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(38,Tok,Stack,Vstack) => 
       case Tok.token() in (
        BAR => 
          do(43,lexer.nextToken(),[38,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(37,Tok,Stack,Vstack) => valof{
      [__t(__T1@isToken(ID(__V1))),..__Vstack] .= Vstack;
      valis do(gotorhs(Stack.head()),Tok,Stack,[__n(I(__V1)),..__Vstack])
    }.
    do(36,Tok,Stack,Vstack) =>
      case Tok.token() in (
        BRACE(_) =>         do(44,lexer.nextToken(),[36,..Stack],[__t(Tok),..Vstack])
      | PREC =>         do(46,lexer.nextToken(),[36,..Stack],[__t(Tok),..Vstack])
      | ID(_) =>         do(47,lexer.nextToken(),[36,..Stack],[__t(Tok),..Vstack])
      | _ => do(gotoaction(36),Tok,[36,..Stack],[__n(EMPTY),..Vstack])
    ).
    do(35,Tok,Stack,Vstack) => valof{
      [_,_,..__Vstack] .= Vstack;
      valis do(gotorule(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(EMPTY),..__Vstack])
    }.
    do(34,Tok,Stack,Vstack) =>
      case Tok.token() in (
        ID(_) =>         do(37,lexer.nextToken(),[34,..Stack],[__t(Tok),..Vstack])
      | ID(_) =>         do(37,lexer.nextToken(),[34,..Stack],[__t(Tok),..Vstack])
      | BRACE(_) => do(gotorhs(34),Tok,[34,..Stack],[__n(EMPTY),..Vstack])
      | SEMI => do(gotorhs(34),Tok,[34,..Stack],[__n(EMPTY),..Vstack])
      | PREC => do(gotorhs(34),Tok,[34,..Stack],[__n(EMPTY),..Vstack])
      | _ => valof{
        scanForToken(BAR);
        valis do(38,lexer.currentToken(),[34,..Stack],Vstack)
      }

    ).
    do(33,Tok,Stack,Vstack) => valof{
      [__n(__V2),__n(__V1),..__Vstack] .= Vstack;
      valis do(gotorules(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(SEQ(__V1,__V2)),..__Vstack])
    }.
    do(32,Tok,Stack,Vstack) => valof{
      [_,__n(__V3),_,__n(__V1),..__Vstack] .= Vstack;
      valis do(gotogpg(drop(Stack,3).head()),Tok,drop(Stack,3),[__n(GPGRULESET(__V1,__V3)),..__Vstack])
    }.
    do(31,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotorules(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(30,Tok,Stack,Vstack) =>
      case Tok.token() in (
        MARK =>         do(32,lexer.nextToken(),[30,..Stack],[__t(Tok),..Vstack])
      | ID(_) =>         do(29,lexer.nextToken(),[30,..Stack],[__t(Tok),..Vstack])

      | _ => valof{
        scanForToken(SEMI);
        valis do(28,lexer.currentToken(),[30,..Stack],Vstack)
      }

    ).
    do(29,Tok,Stack,Vstack) => 
       case Tok.token() in (
        COLON => 
          do(34,lexer.nextToken(),[29,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(28,Tok,Stack,Vstack) => 
       case Tok.token() in (
        SEMI => 
          do(35,lexer.nextToken(),[28,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(27,Tok,Stack,Vstack) => valof{
      [__t(__T2@isToken(CENTBRACE(__V2))),__t(__T1@isToken(ID(__V1))),..__Vstack] .= Vstack;
      valis do(gototokenspec(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(APPLY(__V1,__V2)),..__Vstack])
    }.
    do(26,Tok,Stack,Vstack) => valof{
      [__t(__T2@isToken(BRACE(__V2))),__t(__T1@isToken(ID(__V1))),..__Vstack] .= Vstack;
      valis do(gototokenspec(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(APPLY(__V1,__V2)),..__Vstack])
    }.
    do(25,Tok,Stack,Vstack) => valof{
      [__n(__V2),__n(__V1),..__Vstack] .= Vstack;
      valis do(gototokenspecs(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(SEQ(__V1,__V2)),..__Vstack])
    }.
    do(24,Tok,Stack,Vstack) =>
      case Tok.token() in (
        ID(_) =>         do(19,lexer.nextToken(),[24,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V2),_,..__Vstack] .= Vstack;
      valis do(gototokendef(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(OPERATORSPEC(__V2,nonA)),..__Vstack])
    }
    ).
    do(23,Tok,Stack,Vstack) =>
      case Tok.token() in (
        ID(_) =>         do(19,lexer.nextToken(),[23,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V2),_,..__Vstack] .= Vstack;
      valis do(gototokendef(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(OPERATORSPEC(__V2,rightA)),..__Vstack])
    }
    ).
    do(22,Tok,Stack,Vstack) =>
      case Tok.token() in (
        ID(_) =>         do(19,lexer.nextToken(),[22,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V2),_,..__Vstack] .= Vstack;
      valis do(gototokendef(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(OPERATORSPEC(__V2,leftA)),..__Vstack])
    }
    ).
    do(21,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gototokenspecs(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(20,Tok,Stack,Vstack) =>
      case Tok.token() in (
        ID(_) =>         do(19,lexer.nextToken(),[20,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__n(__V2),_,..__Vstack] .= Vstack;
      valis do(gototokendef(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(TOKENSPEC(__V2)),..__Vstack])
    }
    ).
    do(19,Tok,Stack,Vstack) =>
      case Tok.token() in (
        BRACE(_) =>         do(26,lexer.nextToken(),[19,..Stack],[__t(Tok),..Vstack])
      | CENTBRACE(_) =>         do(27,lexer.nextToken(),[19,..Stack],[__t(Tok),..Vstack])
      | _ =>valof{
      [__t(__T1@isToken(ID(__V1))),..__Vstack] .= Vstack;
      valis do(gototokenspec(Stack.head()),Tok,Stack,[__n(I(__V1)),..__Vstack])
    }
    ).
    do(18,Tok,Stack,Vstack) => valof{
      [__t(__T2@isToken(ID(__V2))),_,..__Vstack] .= Vstack;
      valis do(gotostartdef(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(STARTSPEC(__V2)),..__Vstack])
    }.
    do(17,Tok,Stack,Vstack) => valof{
      [__t(__T2@isToken(INT(__V2))),_,..__Vstack] .= Vstack;
      valis do(gotoexpectdef(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(EXPECTSPEC(__V2)),..__Vstack])
    }.
    do(16,Tok,Stack,Vstack) => valof{
      [__n(__V2),__n(__V1),..__Vstack] .= Vstack;
      valis do(gotopreamble(drop(Stack,1).head()),Tok,drop(Stack,1),[__n(SEQ(__V1,__V2)),..__Vstack])
    }.
    do(15,Tok,Stack,Vstack) =>
      case Tok.token() in (
        ID(_) =>         do(29,lexer.nextToken(),[15,..Stack],[__t(Tok),..Vstack])

      | _ => valof{
        scanForToken(SEMI);
        valis do(28,lexer.currentToken(),[15,..Stack],Vstack)
      }

    ).
    do(14,Tok,Stack,Vstack) =>
      case Tok.token() in (
        EOF => ( yyErrCount>0 ?
                 raise error(yyErrCount.show()<>" parse errors detected",'eFAIL')
               | valof{
                 [__n(__Vx),.._] .= Vstack;
                 valis __Vx;
               }
               )

    ).
    do(13,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotopreamble(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(12,Tok,Stack,Vstack) => 
       case Tok.token() in (
        MARK => 
          do(15,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | TOKEN => 
          do(4,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | LEFT => 
          do(3,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | RIGHT => 
          do(2,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | NONASSOC => 
          do(1,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | CENTBRACE(_) => 
          do(5,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | START => 
          do(6,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | EXPECT => 
          do(7,lexer.nextToken(),[12,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(11,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotodefinition(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(10,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotodefinition(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(9,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotodefinition(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(8,Tok,Stack,Vstack) => valof{
      [__n(__V1),..__Vstack] .= Vstack;
      valis do(gotodefinition(Stack.head()),Tok,Stack,[__n(__V1),..__Vstack])
    }.
    do(7,Tok,Stack,Vstack) => 
       case Tok.token() in (
        INT(_) => 
          do(17,lexer.nextToken(),[7,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(6,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(18,lexer.nextToken(),[6,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(5,Tok,Stack,Vstack) => valof{
      [__t(__T1@isToken(CENTBRACE(__V1))),..__Vstack] .= Vstack;
      valis do(gotoincludedef(Stack.head()),Tok,Stack,[__n(I(__V1)),..__Vstack])
    }.
    do(4,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(19,lexer.nextToken(),[4,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(3,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(19,lexer.nextToken(),[3,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(2,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(19,lexer.nextToken(),[2,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(1,Tok,Stack,Vstack) => 
       case Tok.token() in (
        ID(_) => 
          do(19,lexer.nextToken(),[1,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).
    do(0,Tok,Stack,Vstack) => 
       case Tok.token() in (
        TOKEN => 
          do(4,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | LEFT => 
          do(3,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | RIGHT => 
          do(2,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | NONASSOC => 
          do(1,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | CENTBRACE(_) => 
          do(5,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | START => 
          do(6,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | EXPECT => 
          do(7,lexer.nextToken(),[0,..Stack],[__t(Tok),..Vstack])
      | _ => raise failed
     ).

    popErrorStates:[integer,yyToken,list[integer],list[__item]]=>yyType.
    popErrorStates(Sno,Tok,Stack,Vstack)::Sno in [15,30,34,43] => parse(Sno,Tok,Stack,Vstack).
    popErrorStates(_,Tok,Stack,Vstack) => 
      valof{
        [__nxSno,..__Stack] = Stack;
        valis popErrorStates(__nxSno,Tok,__Stack,Vstack.tail())
      }.


  scanForToken:[yyTokType]*.
  scanForToken(Tok)->
     nxT = lexer.nextToken();
     ( nxT.token()\=Tok ? scanForToken(Tok)).

  }).parse(0,lexer.nextToken(),[],[]).

gotogpg:[integer]=>integer.
gotogpg(0) => 14.

gotopreamble:[integer]=>integer.
gotopreamble(0) => 12.

gotodefinition:[integer]=>integer.
gotodefinition(12) => 16.
gotodefinition(0) => 13.

gototokendef:[integer]=>integer.
gototokendef(12) => 11.
gototokendef(0) => 11.

gototokenspecs:[integer]=>integer.
gototokenspecs(1) => 24.
gototokenspecs(2) => 23.
gototokenspecs(3) => 22.
gototokenspecs(4) => 20.

gototokenspec:[integer]=>integer.
gototokenspec(20) => 25.
gototokenspec(22) => 25.
gototokenspec(23) => 25.
gototokenspec(24) => 25.
gototokenspec(1) => 21.
gototokenspec(2) => 21.
gototokenspec(3) => 21.
gototokenspec(4) => 21.

gotoincludedef:[integer]=>integer.
gotoincludedef(12) => 10.
gotoincludedef(0) => 10.

gotostartdef:[integer]=>integer.
gotostartdef(12) => 9.
gotostartdef(0) => 9.

gotoexpectdef:[integer]=>integer.
gotoexpectdef(12) => 8.
gotoexpectdef(0) => 8.

gotorules:[integer]=>integer.
gotorules(15) => 30.

gotorule:[integer]=>integer.
gotorule(30) => 33.
gotorule(15) => 31.

gotoruleset:[integer]=>integer.
gotoruleset(43) => 49.
gotoruleset(34) => 40.

gotoproduction:[integer]=>integer.
gotoproduction(42) => 50.
gotoproduction(43) => 39.
gotoproduction(34) => 39.

gotorhs:[integer]=>integer.
gotorhs(42) => 36.
gotorhs(43) => 36.
gotorhs(34) => 36.

gotoaction:[integer]=>integer.
gotoaction(48) => 51.
gotoaction(36) => 45.
max:[list[integer],integer]=>integer.
max([],M)=>M.
max([NN,..L],M)::NN>M => max(L,NN).
max([_,..L],M) => max(L,M).
}

