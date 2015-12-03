rss{
  import go.io.
  import go.xml.
  import go.http.
  import go.datelib.
  import go.stdparse.
  import go.setlib.
  import go.disptree.

  rss <~ { disp:[]=>dispTree }.

  private 
  rss:rss..{                            -- this is virtual, should never be used directly
    disp() => s(this.show()).
    show() => this.disp().show().
  }.

  channel(_,_,_,_,_)<=rss.
  channel(title:string,link:uriTp,desc:string,opts:list[rss],items:list[rss]):rss..{
    disp() => n([s("\e[1mChannel \e[22m"),s(title),s("\n\e[1mSource: \e[22m\e[4m"),
                 s(link.show()),
                 s("\e[24m\n"),
                 n({O.disp()..O in opts}),
                 s(desc),s("\n{\n"),
                 n({I.disp()..I in items}),
                 s("}\n")]).
  }.

  guid(_,_)<=rss.
  guid(perm:logical,id:string):rss..{
    disp()::perm=>l(["\e[1mguid: (permalink) \e[22m ",id,"\n"]).
    disp()=>l(["\e[1mguid:\e[22m ",id,"\n"])
  }.

  copyright(_)<=rss.
  copyright(id:string):rss..{
    disp()=>l(["\e[1mcopyright:\e[22m ",id,"\n"])
  }.

  language(_)<=rss.
  language(id:string):rss..{
    disp()=>l(["\e[1mlanguage:\e[22m ",id,"\n"])
  }.

  author(_)<=rss.
  author(id:string):rss..{
    disp()=>l(["\e[1mauthor:\e[22m ",id,"\n"])
  }.

  generator(_)<=rss.
  generator(id:string):rss..{
    disp()=>l(["\e[1mgenerator:\e[22m ",id,"\n"])
  }.

  managingEditor(_)<=rss.
  managingEditor(id:string):rss..{
    disp()=>l(["\e[1mmanaging editor:\e[22m ",id,"\n"])
  }.

  webMaster(_)<=rss.
  webMaster(id:string):rss..{
    disp()=>l(["\e[1mweb master:\e[22m ",id,"\n"])
  }.

  rating(_)<=rss.
  rating(id:string):rss..{
    disp()=>l(["\e[1mrating:\e[22m ",id,"\n"])
  }.

  pubDate(_)<=rss.
  pubDate(d:date):rss..{
    disp()=>l(["\e[1mpubDate:\e[22m ",d.show(),"\n"])
  }.

  lastBuildDate(_)<=rss.
  lastBuildDate(d:date):rss..{
    disp()=>l(["\e[1mlastBuildDate:\e[22m ",d.show(),"\n"])
  }.

  ttl(_)<=rss.
  ttl(N:number):rss..{
    disp()=>l(["\e[1mttl:\e[22m ",N.show(),"\n"])
  }.

  docs(_)<=rss.
  docs(d:uriTp):rss..{
    disp()=>l(["\e[1mdocs:\e[22m ",d.show(),"\n"])
  }.

  comments(_)<=rss.
  comments(d:uriTp):rss..{
    disp()=>l(["\e[1mcomments:\e[22m ",d.show(),"\n"])
  }.

  source(_,_)<=rss.
  source(d:uriTp,cr:string):rss..{
    disp()=>l(["\e[1msource:\e[22m ",d.show(),"\e[1mcredit:\e[22m ",cr,"\n"]).
  }.

  skipHours(_)<=rss.
  skipHours(N:list[number]):rss..{
    disp()=>n([s("\e[1mskipHours:\e[22m "),n({l([I.show()," "])..I in N}),s("\n")])
  }.

  skipDays(_)<=rss.
  skipDays(N:list[dow]):rss..{
    disp()=>n([s("\e[1mskipDays:\e[22m "),n({l([I.show()," "])..I in N}),s("\n")])
  }.

  category(_,_)<=rss.
  category(D:string,N:list[string]):rss..{
    disp()=>n([s("\e[1mcategory:\e[22m "),s(D),s(": "),
               n({l([I.show()," "])..I in N}),s("\n")])
  }.

  enclosure(_,_,_)<=rss.
  enclosure(lk:uriTp,len:integer,mime:string):rss..{
    disp()=>l(["\e[1menclosure:\e[22m ",lk.show()," mime: ",mime,"\n"])
  }.

  image(_,_,_,_,_,_)<=rss.
  image(src:uriTp,title:string,lk:uriTp,desc:string,wid:integer,hght:integer):rss..{
    disp()=>l(["\e[1mimage: ",title,"@ \e[22m ",src.show(),"\n"])
  }.

  cloud(_,_,_,_,_)<=rss.
  cloud(dom:string,port:integer,path:string,reg:string,proto:string):rss..{
    disp()=>l(["\e[1mcloud:\e[22m ",dom,":",port^,"/",path,
               "@@",reg," proto: ",proto,"\n"])
  }.

  item(_,_,_,_)<=rss.
  item(title:string,link:uriTp,desc:string,opts:list[rss]):rss..{
    disp()=>l(["\e[1m",title,"\e[22m{\n\e[4m",link.show(),"\e[24m\n",desc,"\n}\n"]).
  }.


  parseRSS:[xmlDOM]=>rss.

  parseRSS(xmlElement('rss',A,E)):: xmlAtt('version',"2.0") in A 
    => parseChannel(pickElement(E,'channel')).
  parseRSS(D) => raise error("XML structure not RSS 2.0: "<>D.show(),'fail').

  parseChannel:[xmlDOM]=>rss.
  parseChannel(xmlElement('channel',_,E)) => 
      channel(grabText(E,'title',""),
              parseLink(pickElement(E,'link')),
              grabText(E,'description',""),
              parseChannelOpts(E),
              parseChannelItems(E)).

  parseLink:[xmlDOM]=>uriTp.
  parseLink(xmlElement('link',_,L))::xmlText(Lk) in L => parseURI%%Lk.

  
  parseChannelOpts:[list[xmlDOM]]=>list[rss].
  parseChannelOpts([])=>[].
  parseChannelOpts([xmlElement('language',_,E),..L])::xmlText(Lg) in E =>
      [language(Lg),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('copyright',_,E),..L])::xmlText(Lg) in E =>
      [copyright(Lg),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('managingEditor',_,E),..L])::xmlText(Lg) in E =>
      [managingEditor(Lg),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('webMaster',_,E),..L])::xmlText(W) in E =>
      [webMaster(W),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('pubDate',_,E),..L])::xmlText(Dt) in E =>
      [pubDate(parseDate%%Dt),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('lastBuildDate',_,E),..L])::xmlText(Lg) in E =>
      [lastBuildDate(parseDate%%Lg),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('category',A,E),..L])::xmlText(Ct) in E =>
      [category(pickAtt(A,'domain',""),expand(Ct,"/")),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('generator',_,E),..L])::xmlText(Lg) in E =>
      [generator(Lg),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('docs',_,E),..L])::xmlText(Lg) in E =>
      [docs(parseURI%%Lg),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('cloud',A,_),..L]) =>
      [cloud(pickAtt(A,'domain',""),
             natural%%pickAtt(A,'port',"80"),
             pickAtt(A,'path',"/"),
             pickAtt(A,'registerProcedure',""),
             pickAtt(A,'protocol',"")),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('ttl',_,E),..L])::xmlText(Lg) in E =>
      [ttl(numeric%%Lg),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('image',_,E),..L]) =>
      [image(parseURI%%grabText(E,'url',"*"),grabText(E,'title',""),
             parseURI%%grabText(E,'link',"*"),
             grabText(E,'description',""),
             natural%%grabText(E,'width',"88"),
             natural%%grabText(E,'height',"31")),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('rating',_,E),..L])::xmlText(Lg) in E =>
      [rating(Lg),..parseChannelOpts(L)].
  parseChannelOpts([xmlElement('skipHours',_,E),..L]) =>
      [skipHours({natural%%Txt..xmlElement('hour',_,[xmlText(Txt)]) in E}),..
       parseChannelOpts(L)].
  parseChannelOpts([xmlElement('skipDays',_,E),..L]) =>
      [skipDays({parseDow%%Txt..xmlElement('day',_,[xmlText(Txt)]) in E}),..
       parseChannelOpts(L)].
  parseChannelOpts([_,..L])=> parseChannelOpts(L).

  parseChannelItems:[list[xmlDOM]]=>list[rss].
  parseChannelItems([])=>[].
  parseChannelItems([xmlElement('item',_,E),..L]) =>
      [item(grabText(E,'title',""),
            parseURI%%grabText(E,'link',"*"),
            grabText(E,'description',""),
            parseItemOpts(E)),..parseChannelItems(L)].
  parseChannelItems([_,..L])=> parseChannelItems(L).

  parseItemOpts:[list[xmlDOM]]=>list[rss].

  parseItemOpts([])=>[].
  parseItemOpts([xmlElement('author',_,E),..L])::xmlText(Lg) in E =>
      [author(Lg),..parseItemOpts(L)].
  parseItemOpts([xmlElement('category',A,E),..L])::xmlText(Ct) in E =>
      [category(pickAtt(A,'domain',""),expand(Ct,"/")),..parseItemOpts(L)].
  parseItemOpts([xmlElement('comments',_,E),..L])::xmlText(Lg) in E =>
      [comments(parseURI%%Lg),..parseItemOpts(L)].
  parseItemOpts([xmlElement('enclosure',A,_),..L]) =>
      [enclosure(parseURI%%pickAtt(A,'url',"*"),
             natural%%pickAtt(A,'length',"0"),
             pickAtt(A,'type',"")),..parseItemOpts(L)].
  parseItemOpts([xmlElement('guid',A,E),..L])::xmlText(Lg) in E =>
      [guid((pickAtt(A,'isPermaLink',"true")="true"?true|false),Lg),..parseItemOpts(L)].
  parseItemOpts([xmlElement('pubDate',_,E),..L])::xmlText(Dt) in E =>
      [pubDate(parseDate%%Dt),..parseItemOpts(L)].
  parseItemOpts([xmlElement('source',A,E),..L])::xmlText(Ct) in E =>
      [source(parseURI%%pickAtt(A,'url',"*"),Ct),..parseItemOpts(L)].
  parseItemOpts([xmlElement('cloud',A,_),..L]) =>
      [cloud(pickAtt(A,'domain',""),
             natural%%pickAtt(A,'port',"80"),
             pickAtt(A,'path',"/"),
             pickAtt(A,'registerProcedure',""),
             pickAtt(A,'protocol',"")),..parseItemOpts(L)].
  parseItemOpts([xmlElement('ttl',_,E),..L])::xmlText(Lg) in E =>
      [ttl(numeric%%Lg),..parseItemOpts(L)].
  parseItemOpts([xmlElement('rating',_,E),..L])::xmlText(Lg) in E =>
      [rating(Lg),..parseItemOpts(L)].
  parseItemOpts([xmlElement('skipHours',_,E),..L]) =>
      [skipHours({natural%%Txt..xmlElement('hour',_,[xmlText(Txt)]) in E}),..
       parseItemOpts(L)].
  parseItemOpts([xmlElement('skipDays',_,E),..L]) =>
      [skipDays({parseDow%%Txt..xmlElement('day',_,[xmlText(Txt)]) in E}),..
       parseItemOpts(L)].
  parseItemOpts([_,..L])=> parseItemOpts(L).

  parseDow:[dow]-->string.
  parseDow(monday) --> "Monday".
  parseDow(tuesday) --> "Tuesday".
  parseDow(wednesday) --> "Wednesday".
  parseDow(thursday) --> "Thursday".
  parseDow(friday) --> "Friday".
  parseDow(saturday) --> "Saturday".
  parseDow(sunday) --> "Sunday".
  
  grabText:[list[xmlDOM],symbol,string]=>string.
  grabText(L,Ky,_)::xmlElement(Ky,_,T) in L, xmlText(Txt) in T => Txt.
  grabText(_,_,Deflt) => Deflt.

  pickElement:[list[xmlDOM],symbol]=>xmlDOM.
  pickElement([],_)=>raise error("no element",'fail').
  pickElement([E,.._],N)::xmlElement(N,_,_).=E => E.
  pickElement([_,..L],N) => pickElement(L,N).

  pickAtt:[list[xmlAttr],symbol,string]=>string.
  pickAtt([],_,Deflt) => Deflt.
  pickAtt([xmlAtt(Ky,Val),.._],Ky,_)=>Val.
  pickAtt([_,..Atts],Ky,Deflt)=>pickAtt(Atts,Ky,Deflt).

  elementThere:[list[xmlDOM],symbol]{}.
  elementThere([xmlElement(N,_,_),.._],N) :-- true.
  elementThere([_,..L],N) :-- elementThere(L,N).

  rssGet:[string]=>rss.
  rssGet(U) => parseRSS(xmlParse%%httpGet(U,"go-rss/0.0")).

  main(U) ->
      u in U *>
      stdout.outLine("RSS feed from "<>u<>" is \n"<>rssGet(u)^).
}