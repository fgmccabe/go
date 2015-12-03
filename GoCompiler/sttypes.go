sttypes{
  import esc.
  import types.

  standardTypes:dict.

  -- local abbreviations
  private stTp:typeTree = uType('go.stdlib','list',[uType('go.stdlib','char',[])]).
  private thTp:typeTree = uType('go.stdlib','thing',[]).
  private shTp:typeTree = faceType(fields([vS('show',varBind,funType([],stTp))])).

  standardTypes = escTypes.pushList(
   [vS('thing',typeBind, typeDef(thTp, [shTp])),
    vS('exception',typeBind,typeDef(uType('*','exception',[]),
				    [thTp,
				     faceType(fields([vS('show',varBind,funType([],stTp)),
						      vS('cause',varBind,stTp),
						      vS('code',varBind,
							 uType('go.stdlib','symbol',[]))]))])),
    vS('list',typeBind,allType('t',thTp,
			       typeDef(uType('go.stdlib','list',[undef('t')]),
				       [thTp,
					faceType(fields([vS('show',varBind,funType([],stTp)),
							 vS('head',varBind,funType([],undef('t'))),
							 vS('tail',varBind,funType([],uType('go.stdlib','list',[undef('t')]))),
							 vS('eof',varBind,predType([])),
							 vS('cons',varBind,funType([(undef('t'),inpMode)],uType('go.stdlib','list',[undef('t')]))),
							 vS('tack',varBind,funType([(undef('t'),inpMode)],uType('go.stdlib','list',[undef('t')]))),
							 vS('hdtl',varBind,predType([(undef('t'),biMode),(uType('go.stdlib','list',[undef('t')]),biMode)])),
							 vS('eq',varBind,predType([(uType('go.stdlib','list',[undef('t')]),biMode)]))]))]))),
    vS('[]',varBind,allType('t',thTp,conType([],uType('go.stdlib','list',[undef('t')])))),
    vS(',..',varBind,allType('t',thTp,conType([undef('t'),
					       uType('go.stdlib','list',[undef('t')])],
					      uType('go.stdlib','list',[undef('t')])))),
    
    vS(',',typeBind,allType('t',thTp,
			    allType('u',thTp,
				    typeDef(uType('go.stdlib',',',[undef('t'),undef('u')]),
					    [thTp,shTp])))),
    vS(',',varBind,allType('t',thTp,
			   allType('u',thTp,
				   conType([undef('t'),undef('u')],uType('go.stdlib',',',[undef('t'),undef('u')]))))),
    vS('in',varBind,allType('t',thTp,
			    predType([(undef('t'),biMode),(uType('go.stdlib','list',[undef('t')]),biMode)]))),
    vS('true',varBind,conType([],uType('go.stdlib','logical',[]))),
    vS('false',varBind,conType([],uType('go.stdlib','logical',[]))),
    vS('integer',typeBind,typeDef(uType('go.stdlib','integer',[]),
				  [uType('go.stdlib','number',[]),thTp,
				   faceType(fields([vS('show',varBind,funType([],stTp)),
						    vS('equal',varBind,predType([(uType('go.stdlib','number',[]),inpMode)])),
						    vS('less',varBind,predType([(uType('go.stdlib','number',[]),inpMode)]))]))])),
    vS('float',typeBind,typeDef(uType('go.stdlib','float',[]),
				[uType('go.stdlib','number',[]),thTp,
				 faceType(fields([vS('show',varBind,funType([],stTp)),
						  vS('equal',varBind,predType([(uType('go.stdlib','number',[]),inpMode)])),
						  vS('less',varBind,predType([(uType('go.stdlib','number',[]),inpMode)]))]))])),
    vS('number',typeBind,typeDef(uType('go.stdlib','number',[]),
				 [thTp,
				  faceType(fields([vS('show',varBind,funType([],stTp)),
						   vS('equal',varBind,predType([(uType('go.stdlib','number',[]),inpMode)])),
						   vS('less',varBind,predType([(uType('go.stdlib','number',[]),inpMode)]))]))])),
    vS('logical',typeBind,typeDef(uType('go.stdlib','logical',[]), [thTp,shTp])),
    vS('symbol',typeBind,typeDef(uType('go.stdlib','symbol',[]), [thTp,shTp])),
    vS('char',typeBind,typeDef(uType('go.stdlib','char',[]), [thTp,shTp]))]).
}
       


