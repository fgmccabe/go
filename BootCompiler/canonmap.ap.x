/*
 * A module to help canonicalization of Go! programs
 */

#include "go.ah";

module
  import { 
  interface <stdio.af> and
  interface "misc.af" and
  interface "types.af" and
  interface "display.af" and
  interface "dynamic.af" and
  interface "canon.af" and
  interface "findvars.af" and
  interface "refactor.af" and
  interface "errors.af"
} in {

  mungePrefix(m(_,P,_,_,_))=>P;

  mungeName(m(_,P,_,_,_),Nm)=>P++"#"++Nm;

  mungeLocalName(m(_,P,_,_,_),Nm)=>P++"@"++Nm;

  lookupFunName(m(Map,_,_,_,_),Nm) => valof{
    for (Nm,Defn) in Map do{
      if localFun(_,_,_).=Defn || moduleFun(_,_).=Defn then
        valis Defn
    };
    valis notInMap
  };

  lookupRelName(m(Map,_,_,_,_),Nm) => valof{
    for (Nm,Defn) in Map do{
      if localRel(_,_,_).=Defn || moduleRel(_,_).=Defn then
        valis Defn
    };
    valis notInMap
  };

  lookupGrmName(m(Map,_,_,_,_),Nm) => valof{
    for (Nm,Defn) in Map do{
      if localGrm(_,_,_).=Defn || moduleGrm(_,_).=Defn then
        valis Defn
    };
    valis notInMap
  };

  lookupPrcName(m(Map,_,_,_,_),Nm) => valof{
    for (Nm,Defn) in Map do{
      if localPrc(_,_,_).=Defn || modulePrc(_,_).=Defn then
        valis Defn
    };
    valis notInMap
  };

  lookupClassName(m(Map,_,_,_,_),Nm) => valof{
    for (Nm,Defn) in Map do{
      if localClass(_,_).=Defn || moduleClass(_,_,_).=Defn then
        valis Defn
    };
    valis notInMap
  };

  lookupTypeName(m(Map,_,_,_,_),Nm) => valof{
    for (Nm,Defn) in Map do{
      if localType(_).=Defn || moduleType(_,_).=Defn then
        valis Defn
    };
    valis notInMap
  };

  lookupVarName(m(Map,_,_,_,_),Nm) => valof{
    for (Nm,Defn) in Map do{
      if localVar(_,_,_).=Defn || 
      moduleVar(_,_).=Defn || localAsgn(_,_,_).=Defn || moduleAsgn(_,_).=Defn then
        valis Defn
    };
    valis notInMap
  };

  lookupDefnType(m(Map,_,_,_,_),Nm) => valof{
    if (Nm,Defn) in Map then
      valis Defn
    else
      valis notInMap
  };

  lookupNameDef(m(Map,_,_,_,_),Nm) => valof{
    if (Nm,Defn) in Map then
      valis Defn
    else
      valis notInMap
  };

  lookupPackageRef(m(Map,_,_,_,_),Pkg,Nm) => valof{
    if (Nm,Defn::(moduleFun(Pkg,_).=Defn ||
                  moduleRel(Pkg,_).=Defn ||
                  moduleGrm(Pkg,_).=Defn ||
                  modulePrc(Pkg,_).=Defn ||
                  moduleVar(Pkg,_).=Defn ||
                  moduleAsgn(Pkg,_).=Defn ||
                  moduleClass(Pkg,_,_).=Defn ||
                  moduleType(Pkg,_).=Defn)) in Map then
      valis Defn
    else
      valis notInMap
  };

  unpackName(Defn) => valof{
    case Defn in {
      localVar(N,_,_) -> valis N
    | localFun(N,_,_) -> valis N
    | localRel(N,_,_) -> valis N
    | localGrm(N,_,_) -> valis N
    | localPrc(N,_,_) -> valis N
    | localClass(N,_) -> valis N
    | localType(N) -> valis N
    | localAsgn(N,_,_) -> valis N
    | moduleFun(_,N) -> valis N
    | moduleRel(_,N) -> valis N
    | moduleGrm(_,N) -> valis N
    | modulePrc(_,N) -> valis N
    | moduleClass(_,N,_) -> valis N
    | moduleType(_,N) -> valis N
    | moduleVar(_,N) -> valis N
    | moduleAsgn(_,N) -> valis N
    | inherit(N,M,_,_) -> valis N
    | inheritDefn(N,_,_) -> valis N
    }
  };

  defineProg(m(_,_,_,dyn(_,define,_,_),_),Name,Prog){
    define(Name,reFactor(Prog))
  };

  defined(m(_,_,_,dyn(_,_,_,df),_)) => df();

  setProperty(m(_,_,_,_,dyn(_,ds,_,_)),Pr,Vl){
    ds(Pr,Vl);
  };

  delProperty(m(_,_,_,_,dyn(_,ds,cl,_)),Pr){
    cl(Pr);
  };

  getProperty(m(_,_,_,_,dyn(dd,ds,cl,_)),Pr) => dd(Pr);

  isProperty(m(_,_,_,_,dyn(dd,ds,cl,_)),Pr) => valof{
    try{
      _ = dd(Pr);
      valis true
    }
    onerror{
      _ -> valis false
    }
  };

  mapLabel(m(Mp,Nm,Lbl,dQ,_))=>Lbl;

  /* Construct a map of program identifiers for use in translating a class definition */

  trPackageName(L) => L\\((X,Y)=>X++"."++Y);

  setupModuleMap(mName,Imps,Els,Opts) => valof{       -- define the new munged identifiers
    nMap : [];

    for (PkgName,Defs,Ver) in Imps do{
      Pkg = trPackageName(PkgName);
      for (Fld,Mode,Tp) in Defs do{
        case Mode in {
          varBind -> case stripForAlls(Tp) in {
            enuType(_) ->
              nMap := [(Fld,moduleClass(Pkg,Pkg++"#"++Fld,Tp)),..nMap]
          | conType(_,_) ->
              nMap := [(Fld,moduleClass(Pkg,Pkg++"#"++Fld,Tp)),..nMap]
          | funType(_,_) ->
              nMap := [(Fld,moduleFun(Pkg,Pkg++"@"++Fld)),..nMap]
          | predType(_) ->
              nMap := [(Fld,moduleRel(Pkg,Pkg++"@"++Fld)),..nMap]
          | actType(_) ->
              nMap := [(Fld,modulePrc(Pkg,Pkg++"@"++Fld)),..nMap]
          | grType(_,_) ->
              nMap := [(Fld,moduleGrm(Pkg,Pkg++"@"++Fld)),..nMap]
          | _ -> 
              nMap := [(Fld,moduleVar(Pkg,Pkg++"@"++Fld)),..nMap]
          }
        | typeBind -> {}
        }
      }
    };
        
    for node(Defn,mTp,_) in Els do{
      case Defn in {
        fun(Nm,_,Arity,_) -> nMap := [(Nm,moduleFun(mName,mName++"@"++Nm)),..nMap]
      | rel(Nm,_,Arity,_) -> nMap := [(Nm,moduleRel(mName,mName++"@"++Nm)),..nMap]
      | srel(Nm,_,Arity,_) -> nMap := [(Nm,moduleRel(mName,mName++"@"++Nm)),..nMap]
      | grm(Nm,_,Arity,_) -> nMap := [(Nm,moduleGrm(mName,mName++"@"++Nm)),..nMap]
      | prc(Nm,_,Arity,_) -> nMap := [(Nm,modulePrc(mName,mName++"@"++Nm)),..nMap]
      | class(Nm,_,Arity,_) -> nMap := [(Nm,moduleClass(mName,mName++"#"++Nm,mTp)),..nMap]
      | tpe(Nm,_,_) -> nMap := [(Nm,moduleType(mName,mName++"*"++Nm)),..nMap]
      | def(Nm,_,_) -> nMap := [(Nm,moduleVar(mName,mName++"@"++Nm)),..nMap]
      | asg(Nm,_,_) -> nMap := [(Nm,moduleAsgn(mName,mName++"@"++Nm)),..nMap]
      | vlof(_,_) -> {}
      | _ -> exception error("problem in mapClass",'fail)
      }
    };

    valis m(nMap,mName,symb(mName),dynQ([]),dynQ([]));
  };

  -- define the new munged identifiers
  setupClassMap(oMap,node(class(Name,_,clAr,Els),_,cLc)) => valof{ 
    mName = mungePrefix(oMap)++"#"++Name;        -- What is the new munge prefix?

    thisVar = idnt(genNew("this"));
    clVar = idnt(genNew("$cl"));

    m(oMp,_,_,dQ,_).=oMap;

    nMap : oMp;
    Lbl: vdel;

    if node(clss(Q,_,H,Body),Tp,Lc) in Els then{
      for node(Defn,mTp,_) in Body do{
        case Defn in {
          fun(Nm,_,Arity,_) -> 
            nMap := [(Nm,localFun(mName++"@"++Nm,clVar,thisVar)),..nMap]
        | rel(Nm,_,Arity,_) -> 
            nMap := [(Nm,localRel(mName++"@"++Nm,clVar,thisVar)),..nMap]
        | srel(Nm,_,Arity,_) -> 
            nMap := [(Nm,localRel(mName++"@"++Nm,clVar,thisVar)),..nMap]
        | grm(Nm,_,Arity,_) -> 
            nMap := [(Nm,localGrm(mName++"@"++Nm,clVar,thisVar)),..nMap]
        | prc(Nm,_,Arity,_) -> 
            nMap := [(Nm,localPrc(mName++"@"++Nm,clVar,thisVar)),..nMap]
        | class(Nm,_,Arity,_) -> nMap := [(Nm,localClass(mName++"#"++Nm,mTp)),..nMap]
        | tpe(Nm,_,_) -> nMap := [(Nm,localType(mName++"*"++Nm)),..nMap]
        | def(Nm,_,_) -> nMap := [(Nm,localVar(mName++"@"++Nm,clVar,thisVar)),..nMap]
        | asg(Nm,_,_) -> nMap := [(Nm,localAsgn(mName++"@"++Nm,clVar,thisVar)),..nMap]
        | vlof(_,_) -> {}
        | _ -> exception error("problem in mapClass",'fail)
        }
      };
      
      for (Nm,_) in Q do{
        nMap := [(Nm,labelVar),..nMap] -- label vars will cause a copy of the label to be incorporated
      };
      
      if node(enu(Lb),_,_).=H then
        Lbl := symb(mungePrefix(oMap)++"#"++Lb)
      else if node(con(node(iden(",.."),_,_),A),_,_).=H then{
	Lbl := cons(",..",collect{
                      for node(El,eTp,eLc) in A do{
                        if iden(V).=El then
                          elemis idnt(V)
                        else
                          reportError("arguments to labels must be variable, not: "<>showTerm(node(El,eTp,eLc)),eLc)
                      }
                    })
      }
      else if node(con(node(iden(","),_,_),A),_,_).=H then{
	Lbl := cons(",",collect{
                      for node(El,eTp,eLc) in A do{
                        if iden(V).=El then
                          elemis idnt(V)
                        else
                          reportError("arguments to labels must be variable, not: "<>showTerm(node(El,eTp,eLc)),eLc)
                      }
                    })
      }
      else if node(con(node(iden(Lb),lTp,lLc),A),_,_).=H then{
        Lbl := cons(mungePrefix(oMap)++"#"++Lb,collect{
                      for node(El,eTp,eLc) in A do{
                        if iden(V).=El then
                          elemis idnt(V)
                        else
                          reportError("arguments to labels must be variable, not: "<>showTerm(node(El,eTp,eLc)),eLc)
                      }
                    });
      }
      else
        reportError("cannot handle class label: "<>showTerm(H),Lc);
    };

    -- Now that local definitions are in place we handle inheritance
    for node(El,Tp,Lc) in Els do{
      case El in {
        crle(_,_,node(con(node(iden(Spr),_,_),_),uType(SprPkg,_,_),_)) -> {
          faceType(Mthds).=Tp;
	  sName = SprPkg<>"#"<>Spr;
          for (fld,_,_) in Mthds do{
            if !(fld,_) in nMap then
              nMap := [(fld,inheritDefn(mName<>"."<>Spr,clVar,thisVar)),..nMap]
          };
          nMap := [(Spr,inherit(sName,mName<>"."<>Spr,clVar,thisVar)),..nMap]
        }
      | crle(_,_,node(enu(Spr),uType(SprPkg,_,_),_)) -> {
	  sName = SprPkg<>"#"<>Spr;
          faceType(Mthds).=Tp;
          for (fld,_,_) in Mthds do{
            if !(fld,_) in nMap then
              nMap := [(fld,inheritDefn(mName<>"."<>Spr,clVar,thisVar)),..nMap]
          };
          nMap := [(Spr,inherit(sName,mName<>"."<>Spr,clVar,thisVar)),..nMap]
        }
      | _ -> {}
      }
    };

    Map = m(nMap,mName,Lbl,dQ,dynQ([]));
    
    setProperty(Map,'inClass,idnt("true"));
    setProperty(Map,'thisVar,thisVar);
    setProperty(Map,'clVar,clVar);

    valis Map;
  };

  setupAnonClassMap(oMap,Nd::(node(class(Name,_,_,Els),aTp,cLc).=Nd)) => valof{
    m(nMap,mName,aLbl,aQ,aP) = cloneMap(oMap);

    nnMap = collect{
      for (Nme,Entry) in nMap do{
        case Entry in {
          localRel(Lc,clVr,thVr) ->
            elemis (Nme,lblRel(Lc,clVr,thVr))
        | localFun(Lc,clVr,thVr) ->
            elemis (Nme,lblFun(Lc,clVr,thVr))
        | localGrm(Lc,clVr,thVr) ->
            elemis (Nme,lblGrm(Lc,clVr,thVr))
        | localPrc(Lc,clVr,thVr) ->
            elemis (Nme,lblPrc(Lc,clVr,thVr))
        | localAsgn(Lc,clVr,thVr) ->
            elemis (Nme,lblAsgn(Lc,clVr,thVr))
        | localVar(Lc,clVr,thVr) ->
            elemis (Nme,lblVar(Lc,clVr,thVr))
        | inherit(Nm,uNm,clVr,thVr) ->
            elemis (Nme,lblInherit(Nm,uNm,clVr,thVr))
        | inheritDefn(Lc,clVr,thVr) ->
            elemis (Nme,lblInheritDefn(Lc,clVr,thVr))
        | _ -> elemis (Nme,Entry)
        }
      }
    };

    aMap = m([(Name,moduleClass(mName,mName++"#"++Name,aTp)),..nnMap],mName,aLbl,aQ,aP);
    delProperty(aMap,'setLabel);
--    delProperty(aMap,'inClass);
    valis aMap
  };

  cloneMap(m(Mp,Name,Lbl,dQ,dyn(_,_,_,pQ))) => valof{
    nM = m(Mp,Name,Lbl,dQ,dynQ(pQ()));
    delProperty(nM,'setLabel);
    valis nM
  };

  setupSpawnMap(oMap,mName,Lbl,Q) => valof{   -- define an error handling map
    nMap : [];

    for idnt(Nm) in Q do{
      nMap := [(Nm,labelVar),..nMap] -- label vars will cause a copy of the label to be incorporated
    };
      
    m(oMp,_,_,dQ,_).=oMap;

    for (Fld,Df) in oMp do{                      -- We now pick up the package definitions
      if !(Fld,_) in nMap then
        nMap := [(Fld,Df),..nMap];
    };

    Map = m(nMap,mName,Lbl,dQ,dynQ([]));
    
    setProperty(Map,'inClass,idnt("true"));
    setProperty(Map,'thisVar,idnt(genNew("this")));
    setProperty(Map,'clVar,idnt(genNew("$cl")));
    delProperty(Map,'setLabel);

    valis Map;
  };

  showMap(m(Mp,Nm,Lbl,dyn(_,_,_,dQ),dyn(_,_,_,dF))) => valof{
    out : "Map for "<>showCT(Lbl)<>"\n";

    for (N,MM) in Mp do{
      out := out++N++" = "++MM^0++"\n"
    };

    out := out++"Defined programs:\n";
    for (Key,Val) in dQ() do{
      out := out++Key++":\n    "++showCP(Val)++"\n"
    };

    out := out++"Defined properties:\n";
    for (Key,Val) in dF() do{
      out := out++Key^0++":"++showCT(Val)++"\n"
    };
    valis out;
  };

  setupRuleMap(m(Mp,Nm,Lbl,dQ,dyn(_,_,_,dF))) => valof{
    Map = m(Mp,Nm,Lbl,dQ,dynQ(dF()));

    delProperty(Map,'setLabel);
    valis Map
  };

  thVar(Map) => getProperty(Map,'thisVar);
  clVar(Map) => getProperty(Map,'clVar);

  labelAccess(Map::m(Mp,Nm,Lbl,dQ,dyn(dP,dS,_,_)).=Map) => valof{
    try{
      _ = dP('setLabel);
      valis []
    }
    onerror{
      _ -> {
        if symb(_).=Lbl then
          exception error("tried to access variable in non-constructor label",'internal);

        dS('setLabel,Lbl);
        valis [defn(clVar(Map),Lbl,noLoc)]        -- We need to inspect the label
      }
    }
  };

  labelVars(Map::m(Mp,Nm,Lbl,dQ,dyn(dP,dS,_,_)).=Map) => valof{
    try{
      _ = dP('setLabel);

      if cons(_,A).=Lbl then
        valis A<>extraVars(Map)
      else
        valis extraVars(Map)
    }
    onerror{
      _ -> {
        valis extraVars(Map)
      }
    }
  };

  labelArgs(Map::m(Mp,Nm,Lbl,dQ,dyn(dP,dS,_,_)).=Map) => valof{
    try{
      _ = dP('setLabel);

      if cons(_,A).=Lbl then
        valis A
      else
        valis []
    }
    onerror{
      _ -> {
        valis []
      }
    }
  };

  extraVars(Map) => valof{
    if isProperty(Map,'inClass) then
      valis [clVar(Map),thVar(Map)]
    else
      valis []
  };

} export (mungePrefix,lookupDefnType, mungeName, mungeLocalName,
          thVar, clVar, defineProg, defined,
	  labelAccess, labelArgs,labelVars, extraVars,
          getProperty,setProperty,delProperty,isProperty, mapLabel,
          lookupNameDef,unpackName, lookupFunName,lookupRelName,lookupGrmName,
          lookupPrcName, lookupClassName, lookupTypeName, lookupVarName,lookupPackageRef,
          setupModuleMap, setupClassMap, setupAnonClassMap, 
          setupSpawnMap,setupRuleMap,showMap,cloneMap)
