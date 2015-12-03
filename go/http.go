/*
   Implement a simple http 1.1 GET request protocol
   (c) 2004-2005 F.G. McCabe
 
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

go.http{
  import go.io.
  import go.xml.
  import go.stdparse.
  import go.datelib.
  import go.setlib.

  hostTp <~ thing.
    
  urlTp ::= nullURI.

  url:[hostTp,string,string]@=urlTp.
  url(h,path,query)..{
    show()::query=="" => "http://"<>h.show()<>"/"<>path.
    show() => h.show()<>"/"<>path<>"/"<>query.
  }.

  filePath:[string,string]@=urlTp.
  filePath(path,query)..{
    show()::query==[]=>"file://"<>path.
    show()=>collapse([path,query],"?").
  }.

  host:[list[string],integer]@=hostTp.
  host(dns,port)..{
    show()=> collapse(dns,".")<>":"<>port.show().
  }.
  
  httpCode ::=
      httpContinue              -- 100
    | httpOk			-- 200
    | httpCreated 		-- 201
    | httpAccepted		-- 202
    | httpNoContent		-- 204
    | httpMovedPermanently	-- 301
    | httpMovedTemporarily	-- 302
    | httpNotModified		-- 304
    | httpBadRequest		-- 400
    | httpUnauthorized		-- 401
    | httpForbidden		-- 403
    | httpNotFound		-- the dreaded 404
    | httpServerError		-- 500
    | httpNotImplemented	-- 501
    | httpBadGateway		-- 502
    | httpServiceUnavailable.	-- 503

  cacheAtt ::=
          noCache
        | noStore
        | maxAge(float)
        | smaxAge(float)
        | maxStale(float)
        | minFresh(float)
        | onlyIfCached
        | noTransform
        | publicCache
        | privateCache
        | mustRevalidate
        | proxyRevalidate.

      
  entityTag ::= anyEntity | eTag(logical,string).
  
  rangeSpec ::= betweenRange(integer,integer) | endRange(integer).
  
  headerParam::=
      qualityParam(float)
    | attVal(string,string).
    
  productToken::=
      product(string,string)
    | productCmnt(string).
      
  tCoding::=
      trailers
    | chunked
    | tExtension(string,list[headerParam]).
    
  warningTp::=
    warn(integer,hostTp,string,date).
    
  headerAtt ::=
      acceptMedia(list[(string,string,list[headerParam])])
    | acceptCharset(list[(string,list[headerParam])])
    | acceptEncoding(list[(string,list[headerParam])])
    | acceptLanguage(list[(string,list[headerParam])])
    | acceptRanges(string)
    | authorization(list[string])
    | cookie(list[(string,string)])
    | ageIs(float)
    | allowed(list[string])
    | cacheControl(list[cacheAtt])
    | connection(list[string])
    | contentEncoding(list[string])
    | contentLanguage(list[(string,list[headerParam])])
    | contentLength(integer)
    | contentLocation(string)
    | contentMD5(string)
    | contentRange(integer,integer,integer)
    | contentType(string,string,list[headerParam])
    | dateIs(date)
    | etag(entityTag)
    | expect(list[string])
    | expires(date)
    | fromEmail(string)
    | hostName(hostTp)
    | ifMatch(list[entityTag])
    | ifModifiedSince(date)
    | ifNoneMatch(list[entityTag])
    | ifERange(entityTag)
    | ifDRange(date)
    | ifUnModifiedSince(date)
    | keepAlive(string)
    | lastModified(date)
    | location(string)
    | maxForwards(integer)
    | pragma(list[(string,string)])
    | proxyAuthenticate(list[string])
    | proxyAuthorize(list[string])
    | proxyWarning(integer,string,string)
    | trailerHeader(headerAtt)
    | transferEncoding(list[string])
    | viaProxy(string,string)
    | byteRange(list[rangeSpec])
    | referer(string)
    | retryAfter(date)
    | retryAfterDelta(float)
    | serverId(list[productToken])
    | teReq(list[tCoding])
    | trailFields(list[string])
    | transferCoding(list[tCoding])
    | upgradeProtocol(list[productToken])
    | userAgent(list[productToken])
    | vary(list[string])
    | via(list[(string,string,string,integer,string)])
    | warningHeader(list[warningTp])
    | wwwAuthenticate(list[string])
    | otherHeader(string,string).


  httpVersion:[float] --> string.
  httpVersion(V) --> "HTTP",skip(),"/",floatOf(V).
  
  httpResponseLine:[float,httpCode,string]-->string.
  httpResponseLine(V,C,Msg) --> httpVersion(V),skip(),parseHttpCode(C), grabAll(Msg).

  parseHttpCode:[httpCode]-->string.
  parseHttpCode(httpContinue) --> "100".
  parseHttpCode(httpOk) --> "200".
  parseHttpCode(httpCreated) --> "201".
  parseHttpCode(httpAccepted) --> "202".
  parseHttpCode(httpNoContent) --> "204".
  parseHttpCode(httpMovedPermanently) --> "301".
  parseHttpCode(httpMovedTemporarily) --> "302".
  parseHttpCode(httpNotModified) --> "304".
  parseHttpCode(httpBadRequest) --> "400".
  parseHttpCode(httpUnauthorized) --> "401".
  parseHttpCode(httpForbidden) --> "403".
  parseHttpCode(httpNotFound) --> "404".
  parseHttpCode(httpServerError) --> "500".
  parseHttpCode(httpNotImplemented) --> "501".
  parseHttpCode(httpBadGateway) --> "502".
  parseHttpCode(httpServiceUnavailable)-->"503".

  private token:[string]-->string.
  token([C,..L]) --> "%",hexSeq(H,2),C=__charOf(H),more_token(L).
  token([C,..L]) --> [C],{\+separatorCtl(C)},more_token(L).
  
  private more_token:[string]-->string.
  more_token(L) --> token(L).
  more_token([])--> [].
  
  private quoted_string:[string]-->string.
  quoted_string(L) --> "\"",more_string(`",L).
  
  private comment:[string]-->string.
  comment(L) --> "(",more_string(`),L).
  
  private more_string:[char,string]-->string.
  more_string(C,[])-->[C].
  more_string(X,[C,..L]) --> "\\",[C],more_string(X,L).
  more_string(X,[C,..L]) --> [C],{C!=X},more_string(X,L).
  
  private grabAll:[string]-->string.
  grabAll([C,..L]) --> [C],grabAll(L).
  grabAll([]) --> [].
    
  private separatorCtl:[char]{}.
  separatorCtl(`().
  separatorCtl(`)).
  separatorCtl(`<).
  separatorCtl(`>).
  separatorCtl(`@).
  separatorCtl(`,).
  separatorCtl(`;).
  separatorCtl(`:).
  separatorCtl(`\\).
  separatorCtl(`").
  separatorCtl(`{).
  separatorCtl(`}).
  separatorCtl(`[).
  separatorCtl(`]).
  separatorCtl(`/).
  separatorCtl(`?).
  separatorCtl(`=).
  separatorCtl(` ).
  separatorCtl(`\t).

  skip:[]-->string.
  skip()-->" ", skip().
  skip()-->"\t",skip().
  skip()-->"".
  
  private parseDate:[date]-->string.
  parseDate(Date) --> rfc822_date(Date),skip().  -- These are defined in stdparse.gof
--  parseDate(Date) --> rfc850_date(Date).
--  parseDate(Date) --> asctime_date(Date).
  
  combineHeaderLines:[list[string]]=>list[string].
  combineHeaderLines([])=>[].
  combineHeaderLines([H,..M]) => mergeHeader(H,M).
  
  private mergeHeader:[string,list[string]]=>list[string].
  mergeHeader(H,[])=>[H].
  mergeHeader(H,[[Ch,..L],..M])::(Ch=` |Ch=`\t) => mergeHeader(H<>" "<>L,M).
  mergeHeader(H,M) => [H,..combineHeaderLines(M)].
  
  -- parse header attributes
  parseHeaders:[list[string]]=>list[headerAtt].
  parseHeaders([]) => [].
  parseHeaders([H,..M]) => [parseHeader%%H,..parseHeaders(M)].
  
  private parseHeader:[headerAtt]-->string.
  parseHeader(acceptMedia(Media)) --> "Accept",skip(), ":", skip(), parseMedia(a,b,c)*(a,b,c)^Media.
  parseHeader(acceptCharset(Chsets)) --> "Accept-Charset",skip(), ":", skip(), parseCharsets(Chsets).
  parseHeader(acceptEncoding(Encsets)) --> "Accept-Encoding",skip(), ":", skip(), parseEncodings(Encsets).
  parseHeader(acceptLanguage(Sets)) --> "Accept-Language",skip(), ":", skip(), 
      parseLanguage(a,b)*(a,b)^Sets.
  parseHeader(acceptRanges(Sets)) --> "Accept-Ranges",skip(), ":", skip(), parseRanges(Sets).
  parseHeader(ageIs(age)) --> "Age",skip(), ":" , skip(), floatOf(age).
  parseHeader(allowed(methods)) --> "Allow", skip(), ":" , skip(), token(X)*X^methods.
  parseHeader(authorization(credentials)) --> "Authorization", skip(), ":" , skip(), 
      token(X)*X^credentials.
  parseHeader(cacheControl(directives)) --> "Cache-Control", skip(), ":" , skip(), 
      parseCacheDirective(X)*X^directives.
  parseHeader(connection(Modes)) --> "Connection",skip(), ":", skip(), token(X)*X^Modes.
  parseHeader(contentEncoding(Modes)) --> "Content-encoding",skip(), ":", skip(), token(X)*X^Modes.
  parseHeader(contentLanguage(L)) --> "Content-Language",skip(), ":", skip(), 
      parseLanguage(a,b)*(a,b)^L.
  parseHeader(contentLength(L)) --> "Content-Length",skip(), ":", skip(), integerOf(L).
  parseHeader(contentLocation(L)) --> "Content-Location",skip(), ":", skip(), token(L).
  parseHeader(contentMD5(L)) --> "Content-MD5",skip(), ":", skip(), token(L).
  parseHeader(contentRange(F,L,N)) --> "Content-Range",skip(), ":", skip(),"bytes",skip(), 
      integerOf(F),skip(),"-",skip(),integerOf(L),skip(),"/",skip(),integerOf(N).
  parseHeader(contentType(T,S,Q)) --> "Content-Type",skip(), ":", skip(), parseMedia(T,S,Q).
  parseHeader(dateIs(D)) --> "Date",skip(),":",skip(),parseDate(D).
  parseHeader(etag(T)) --> "ETag",skip(),":",skip(),parseEntityTag(T).
  parseHeader(expect(E)) --> "Expect",skip(),":",skip(),token(X)*X^E.
  parseHeader(expires(D)) --> "Expires",skip(),":",skip(),parseDate(D).
  parseHeader(fromEmail(E)) --> "From",skip(),":",skip(),token(E).
  parseHeader(hostName(H)) --> "Host",skip(),":",skip(),parseHost(H).
  parseHeader(ifMatch(L)) --> "If-Match",skip(),":",skip(),("*",L=[anyEntity]|parseEntityTag(X)*X^L).
  parseHeader(ifModifiedSince(D)) --> "If-Modified-Since",skip(),":",skip(),parseDate(D).
  parseHeader(ifNoneMatch(L)) --> "If-None-Match",skip(),":",skip(),
      ("*",L=[anyEntity]|parseEntityTag(X)*X^L).
  parseHeader(ifERange(I)) --> "If-Range",skip(),":",skip(),parseEntityTag(I).
  parseHeader(ifDRange(I)) --> "If-Range",skip(),":",skip(),parseDate(I).
  parseHeader(ifUnModifiedSince(D)) --> "If-Unmodified-Since",skip(),":",skip(),parseDate(D).
  parseHeader(keepAlive(D)) --> "Keep-Alive",skip(),":",skip(),token(D).
  parseHeader(lastModified(D)) --> "Last-Modified",skip(),":",skip(),parseDate(D).
  parseHeader(location(L)) --> "Location",skip(), ":", skip(), token(L).
  parseHeader(maxForwards(L)) --> "Max-Forwards",skip(), ":", skip(), integerOf(L).
  parseHeader(pragma(L)) --> "Pragma",skip(),":",skip(),parsePragma(a,b)*(a,b)^L.
  parseHeader(proxyAuthenticate(L)) --> "Proxy-Authenticate",skip(),":",skip(),token(X)*X^L.
  parseHeader(proxyAuthorize(L)) --> "Proxy-Authorization",skip(),":",skip(),token(X)*X^L.
  parseHeader(byteRange(Spec)) --> "Range",skip(),":",skip(),parseRange(X)*X^Spec.
  parseHeader(referer(E)) --> "Referer",skip(),":",skip(),token(E).
  parseHeader(retryAfter(D)) --> "Retry-After",skip(),":",skip(),parseDate(D).
  parseHeader(retryAfterDelta(D)) --> "Retry-After",skip(),":",skip(),floatOf(D).
  parseHeader(serverId(P)) --> "Server",skip(),":",skip(),parseProduct(X)*X^P.
  parseHeader(teReq(L)) --> "TE",skip(),":",skip(),parseTcoding(X)*X^L.
  parseHeader(trailFields(L)) --> "Trailers",skip(),":",skip(),token(X)*X^L.
  parseHeader(transferCoding(L)) --> "Transfer-Encoding",skip(),":",skip(),parseTcoding(X)*X^L.
  parseHeader(upgradeProtocol(P)) --> "Upgrade",skip(),":",skip(),parseProduct(X)*X^P.
  parseHeader(userAgent(P)) --> "User-Agent",skip(),":",skip(),parseProduct(X)*X^P.
  parseHeader(vary(L)) --> "Vary",skip(),":",skip(),token(X)*X^L.
  parseHeader(via(L)) --> "Via",skip(),":",skip(),parseVia(a,b,c,d,e)*(a,b,c,d,e)^L.
  parseHeader(warningHeader(L)) --> "Warning",skip(),":",skip(),parseWarning(X)*X^L.
  parseHeader(wwwAuthenticate(challenges)) --> "WWW-Authenticate", skip(), ":" , skip(), token(X)*X^challenges.
  parseHeader(otherHeader(key,Val)) --> token(key),skip(),":",skip(),grabAll(Val).
  
  private parseMedia:[string,string,list[headerParam]]-->string.
  parseMedia(Type,subType,Q) --> 
     token(Type),skip(),"/",skip(),token(subType),skip(),parseParams(Q).

  private parseCharsets:[list[(string,list[headerParam])]]-->string.
  parseCharsets([(Type,Q),..More]) --> 
     token(Type), skip(),parseParams(Q), skip(), (",",skip(),parseCharsets(More) | More=[]).

  private parseEncodings:[list[(string,list[headerParam])]]-->string.
  parseEncodings([(Type,Q),..More]) --> 
     token(Type), skip(),parseParams(Q), skip(), (",",skip(),parseEncodings(More) | More=[]).

  private parseLanguage:[string,list[headerParam]]-->string.
  parseLanguage(Type,Q) --> 
     token(Type), skip(),parseParams(Q).

  private parseRanges:[string]-->string.
  parseRanges(Tk) --> token(Tk).

  private parseParams:[list[headerParam]]-->string.
  parseParams([qualityParam(Q),..More]) --> ";",skip(),"q",skip(),"=",floatOf(Q),parseParams(More).
  parseParams([attVal(Att,Val),..More]) --> ";",skip(),token(Att),skip(),"=",token(Val), parseParams(More).
  parseParams([]) --> [].
  
  private parseProduct:[productToken]-->string.
  parseProduct(productCmnt(L)) --> comment(L).
  parseProduct(product(Name,Type)) --> token(Name), skip(), ("/",skip(),token(Type),skip() | Type="").
  
  private parseCacheDirective:[cacheAtt]-->string.
  parseCacheDirective(noCache) --> "no-cache".
  parseCacheDirective(noStore) --> "no-store".
  parseCacheDirective(publicCache) --> "public".
  parseCacheDirective(privateCache) --> "private".
  parseCacheDirective(onlyIfCached) --> "only-if-cached".
  parseCacheDirective(maxAge(N)) --> "max-age",skip(),"=",skip(),floatOf(N).
  parseCacheDirective(smaxAge(N)) --> "s-max-age",skip(),"=",skip(),floatOf(N).
  parseCacheDirective(maxStale(N)) --> "max-stale",skip(),"=",skip(),floatOf(N).
  parseCacheDirective(minFresh(N)) --> "min-fresh",skip(),"=",skip(),floatOf(N).
  parseCacheDirective(minFresh(N)) --> "min-fresh",skip(),"=",skip(),floatOf(N).
  parseCacheDirective(mustRevalidate) --> "must-revalidate".
  parseCacheDirective(proxyRevalidate) --> "proxy-revalidate".
  
  private parseEntityTag:[entityTag]-->string.
  parseEntityTag(eTag(true,Tag)) --> "W/",skip(),quoted_string(Tag).
  parseEntityTag(eTag(false,Tag)) --> quoted_string(Tag).
  
  private parsePragma:[string,string]-->string.
  parsePragma("cache","no") --> "no-cache".
  parsePragma(P,V) --> token(P),skip(),"=",skip(),(token(V)|quoted_string(V)).
  
  private parseRange:[rangeSpec]-->string.
  parseRange(betweenRange(F,T)) --> integerOf(F),skip(),"-",skip(),integerOf(T).
  parseRange(endRange(T)) --> "-",skip(),integerOf(T).
  
  private parseTcoding:[tCoding]-->string.
  parseTcoding(trailers) --> "trailers".
  parseTcoding(tExtension(N,Q)) --> token(N),skip(),parseParams(Q).
  
  private parseVia:[string,string,string,integer,string]-->string.
  parseVia(Proto,Ver,Host,Port,Cmnt) --> 
      token(P),skip(),("/",Proto=P,token(Ver),skip() | Proto="HTTP",Ver=P),
      token(Host),skip(),(":",naturalOf(Port),skip() | Port=80),
      (comment(Cmnt) | Cmnt="").
              
  private parseWarning:[warningTp]-->string.
  parseWarning(warn(Code,Host,Msg,Date)) --> integerOf(Code),skip(),parseHost(Host),skip(),quoted_string(Msg),skip(),
              "\"",skip(),parseDate(Date),skip(),"\"".
    
  keepingAlive:[list[headerAtt]]{}.
  keepingAlive(H) :- keepAlive("keep-alive") in H.
  keepingAlive(H) :- connection(M) in H, "keep-alive" in M.
  keepingAlive(H) :- connection(M) in H, \+"close" in M.
  
  parseURI:[urlTp]-->string.
  parseURI(nullURI) --> "*".
  parseURI(Uri) --> absoluteURI(Uri).
  parseURI(filePath([`/,..File],Q)) --> "/",parsePath(File),("?",Qury(Q) | Q=[]).
  parseURI(nullURI) --> "*".
  
  absoluteURI:[urlTp]-->string.
  absoluteURI(url(hst,path,query)) --> "http://",parseHost(hst),
                           ("/",parsePath(path), ("?",Qury(query) | query=[]) |
                            path=[], query=[]).
    
  parseHost:[hostTp]-->string.
  parseHost(host(H,P)) --> dotted(H),(":",naturalOf(P)|P=80).
  
  private dotted:[list[string]]-->string.
  dotted([D,..L]) --> token(D),(".",dotted(L) | L=[]).
    
  parsePath:[string]-->string.
  parsePath(O) --> token(seg)!,("/",parsePath(more),M=[`/,..more]|M=""),O=seg<>M.
  parsePath([]) --> "".
  
  Qury:[string]-->string.
  Qury([C,..L]) --> "%",hexSeq(H,2),C=__charOf(H),Qury(L). 
  Qury([C,..L]) --> [C],{\+whiteSpace(C)},Qury(L).
  Qury([]) --> "".
  
  /* White space */
  whiteSpace:[char]{}.
  whiteSpace(X)::__isZsChar(X) :-- {}.
  whiteSpace(X)::__isZlChar(X) :-- {}.
  whiteSpace(X)::__isZpChar(X) :-- {}.
  whiteSpace(X)::__isCcChar(X) :-- {}.
  
  httpErrCode:[httpCode]=>string.
  httpErrCode(httpContinue) => "100".
  httpErrCode(httpOk) => "200".
  httpErrCode(httpCreated) => "201".
  httpErrCode(httpAccepted) => "202".
  httpErrCode(httpNoContent) => "204".
  httpErrCode(httpMovedPermanently) => "301".
  httpErrCode(httpMovedTemporarily) => "302".
  httpErrCode(httpNotModified) => "304".
  httpErrCode(httpBadRequest) => "400".
  httpErrCode(httpUnauthorized) => "401".
  httpErrCode(httpForbidden) => "403".
  httpErrCode(httpNotFound) => "404".
  httpErrCode(httpServerError) => "500".
  httpErrCode(httpNotImplemented) => "501".
  httpErrCode(httpBadGateway) => "502".
  httpErrCode(httpServiceUnavailable) => "503".


  private grabResponse:[inChannel,string,list[string]]*.
  grabResponse(i,M,A) -> grabMain(i,M); grabAtts(i,A).

  private readALine:[inChannel]=>string.
  readALine(i) => stripCr(i.inLine("\n")).

  private stripCr:[string]=>string.
  stripCr([]) => [].
  stripCr([`\r]) => [].
  stripCr([X,..L]) => [X,..stripCr(L)].

  grabMain:[inChannel,string]*.
  grabMain(i,M) -> M = readALine(i).
  
  grabAtts:[inChannel,list[string]]*.
  grabAtts(i,Atts) -> Line = readALine(i);
      (Line==[] ? Atts = [] | Atts = [Line,..A1]; grabAtts(i,A1)).

  private readResponse:[inChannel,outChannel,httpCode,list[headerAtt],string]*.
  readResponse(i,_,Cmd,Hdrs,Msg) ->
      grabResponse(i,M,A);
      { httpResponseLine(V,Cmd,Msg) --> M };
      Hdrs = parseHeaders(combineHeaderLines(A));
      stdout.outLine("Response = "<>Cmd^<>" http version "<>V^<>" "<>Msg);
      (Hd in Hdrs *> stdout.outLine(Hd^)).

  -- Implement the HTTP GET verb
  httpGet:[string,string]=>string.

  httpGet(Url,Agent) => valof{
                          case parseURI%%Url in (
                           url(host(DNS,port),path,query) -> 
                               h = collapse(DNS,".");
                               tcpConnect(h,port,i,o,utf8Encoding);
                               o.outLine("GET /"<>path<>addQuery(query)<>" HTTP/1.1");
                               o.outLine("Accept: text/xml, text/*");
                               o.outLine("User-agent: "<>Agent);
                               o.outLine("Host: "<>h);
                               o.outLine("");
                               readResponse(i,o,Code,Hdrs,Msg);
                               valis handleGetResponse(Code,Hdrs,Msg,i);
                               o.close();
                               i.close()
                         | filePath(path,_) ->
                               stdout.outLine("Opening "<>path);
                               I = openInFile(path,unknownEncoding);
                               valis I.inText("");
                               I.close()
                          | nullURI ->
                               raise error("nothing to be found at a null URI",'fail')
                          )
                        }.

  private addQuery:[string]=>string.
  addQuery([])=>[].
  addQuery(Q)=>[`/,..Q].

  private handleGetResponse:[httpCode,list[headerAtt],string,inChannel] => string.
  handleGetResponse(httpOk,Hdrs,_,i)::contentLength(L) in Hdrs => i.inChars(L).
  handleGetResponse(httpOk,_,_,i) => i.inText("").
  handleGetResponse(E,_,M,_) => 
      valof{
        stdout.outLine("Error: "<>httpErrCode(E)<>M);
        valis ""
      }.
  
  main(U) ->
      u in U *>
      stdout.outLine("XML text from "<>u<>" is \n"<>(xmlParse%%httpGet(u,"go/0.0"))^).
}      
