t{
  thing:[]$=thing.
  thing..{
    show()::nonvar(this),__getProp(this,'$label',Lb) => explode(Lb).
    show() => __stringOf(this,0,0).
  }.

  false..{
    show() => S(this).
  }.

  true..{
    show() => "true".
  }.

  f:[logical]$=logical.
  f(T)..{
    show()=>T.show()
  }.

  T:logical = true.

  S:[logical]=>string.
  S(true)=>"true".
  S(false)=>"false".
}

