/*
 * A sample web server, written in Go!.
 * Structure modeled after aprilweb. Use at your own risk!
 */
 
goweb{
  import go.io.
  import go.stdparse.
  import go.datelib.
  import go.setlib.
  import go.http.

  httpDir:string = getenv('HTTP_home',fcwd()<>"/").

  responseType ::= simpleResponse | fullResponse.
  
  httpRequest::=
      httpGet(urlTp)
    | httpPut(urlTp)
    | httpPost(urlTp)
    | httpOptions(urlTp)
    | httpTrace(urlTp)
    | httpHead(urlTp)
    | httpDelete(urlTp)
    | httpConnect(urlTp).
  
  MimeTypes:list[(string,string,string)] = [
       ("bin",     "application", "octet-stream"),
       ("pdf",     "application", "pdf"),  -- MIME types: As taken from Netscape 4.05 on Windows
       ("aif",     "audio", "x-aiff"),
       ("aiff",    "audio", "x-aiff"),
       ("aifc",    "audio", "x-aiff"),
       ("aim",     "application", "x-aim"),
       ("ras",     "image", "x-cmu-raster"),
       ("x",       "application", "x-compress"),
       ("xlc",     "application", "xlc"),
       ("gif",     "image", "gif"),
       ("texi",    "application", "x-texinfo"),
       ("texinfo", "application", "x-texinfo"),
       ("gz",      "application", "x-gzip"),
       ("htm",     "text", "html"),
       ("html",    "text", "html"),
       ("js",      "application", "x-javascript"),
       ("mocha",   "application", "x-javascript"),
       ("jpeg",    "image", "jpeg"),
       ("jpg",     "image", "jpeg"),
       ("jpe",     "image", "jpeg"),
       ("jfif",    "image", "jpeg"),
       ("pjpeg",   "image", "jpeg"),
       ("pjp",     "image", "jpeg"),
       ("hqx",     "application", "mac-binhex40"),
       ("avi",     "video", "x-msvideo"),
       ("mp2",     "audio", "x-mpeg"),
       ("mpa",     "audio", "x-mpeg"),
       ("abs",     "audio", "x-mpeg"),
       ("mpega",   "audio", "x-mpeg"),
       ("pbm",     "image", "x-portable-bitmap"),
       ("pgm",     "image", "x-portable-graymap"),
       ("pcd",     "image", "x-photo-cd"),
       ("txt",     "text", "plain"),
       ("text",    "text", "plain"),
       ("png",     "image", "png"),
       ("ppt",     "application", "ppt"),
       ("pot",     "application", "pot"),
       ("pps",     "application", "pps"),
       ("ppm",     "image", "x-portable-bitmap"),
       ("enc",     "application", "pre-encrypted"),
       ("qt",      "video", "quicktime"),
       ("mov",     "video", "quicktime"),
       ("moov",    "video", "quicktime"),
       ("tiff",    "image", "tiff"),
       ("au",      "audio", "basic"),
       ("snd",     "audio", "basic"),
       ("tar",     "application", "x-tar"),
       ("wav",     "audio", "x-wav"),
       ("bmp",     "image", "x-MS-bmp"),
       ("rtf",     "application", "rtf"),
       ("doc",     "application", "msword"),
       ("tgz",     "application", "x-compressed"),
       ("xbm",     "image", "x-bitmap"),
       ("zip",     "application", "x-zip-compressed")].
  
  errorTitle:[httpCode,string]=>(string,string).
  errorTitle(httpOk,_) => ("OK","").
  errorTitle(httpCreated,_) => ("Created","").
  errorTitle(httpAccepted,_) => ("Accepted","").
  errorTitle(httpNoContent,A) => ("No content",A).
  errorTitle(httpMovedTemporarily,A) => ("Moved temporarily","The document has moved <a HREF=\""<>A<>"\"> here</a>.").
  errorTitle(httpNotFound,A) => ("Not found","We don't have "<>A).
  errorTitle(httpBadRequest,R) => ("Bad Request","Your browser sent me a request: '"<>R<>"',that I could not grok").
  errorTitle(httpForbidden,A) => ("Forbidden","You do not have permission to access "<>A).
  errorTitle(httpServerError,_) => ("Server Error","Internal or misconfiguration error").
  errorTitle(httpNotImplemented,A) => ("Not implemented", A<>" not supported").
  
  errorResponse:[httpCode,string,responseType]=>string.
  errorResponse(ErrCode,ErrArgs,simpleResponse)::(ErrorTitle,ErrorText)=errorTitle(ErrCode,ErrArgs) =>
      "<html><head>\n<title>"<>httpErrCode(ErrCode)<>" "<>ErrorTitle<>"\n"<>
      "</title>\n</head><body>\n<h1>"<>ErrorTitle<>"</h1>\n<p>"<>ErrorText<>"</p>\n"
      "<hr><address>GoWeb/0.0 Server </address>\n</body></html>\n".
  errorResponse(ErrCode,ErrArgs,fullResponse):: (ErrorTitle,_)=errorTitle(ErrCode,ErrArgs),
  errorBody = errorResponse(ErrCode,ErrArgs,simpleResponse) =>
      "HTTP/1.1 "<>httpErrCode(ErrCode)<>" "<>ErrorTitle<>"\n"<>
      "Date: "<>time2utc(now()).show()<>"\n"<>
      "Server: GoWeb 0.0\n"<>
      "Allow: GET HEAD\n"<>
      "Connection: close\n"<>
      "Content-Length: "<>listlen(errorBody).show()<>"\n"<>
      "Content-type: text/html\n\n"<>
      errorBody<>"\n".
        
  reportError:[httpCode,string,responseType,integer,outChannel]*.
  reportError(ErrCode,ErrArgs,rType,Count,out) -> 
      ErrText = errorResponse(ErrCode,ErrArgs,rType);
      out.outStr(ErrText);
      Count=listlen(ErrText).
      
  grabRequest:[inChannel,string,list[string]]*.
  grabRequest(i,M,A) -> grabMain(i,M); grabAtts(i,A).

  requestURI:[urlTp]-->string.
  requestURI(nullURI) --> "*".
  requestURI(Uri) --> absoluteURI(Uri).
  requestURI(filePath(File,Q)) --> "/",parsePath(File),("?",Qury(Q) | Q=[]).
  
  showFilePath:[urlTp]=>string.
  showFilePath(filePath(File,[])) => File.
  showFilePath(filePath(File,Q)) => File<>"?"<>Q.
  
  absoluteURI:[urlTp]-->string.
  absoluteURI(url(hst,path,query)) --> "http://",parseHost(hst),
                           ("/",parsePath(path), ("?",Qury(query) | query=[]) |
                            path=[], query=[]).
    
  -- parse the message line

  httpRequestLine:[httpRequest,float]-->string.
  httpRequestLine(httpGet(root),V) --> "GET",skip(),requestURI(root),skip(),httpVersion(V).
  httpRequestLine(httpHead(root),V) --> "HEAD",skip(),requestURI(root),skip(),httpVersion(V).
  httpRequestLine(httpPost(root),V) --> "POST",skip(),requestURI(root),skip(),httpVersion(V).
  httpRequestLine(httpPut(root),V) --> "PUT",skip(),requestURI(root),skip(),httpVersion(V).
  httpRequestLine(httpDelete(root),V) --> "DELETE",skip(),requestURI(root),skip(),httpVersion(V).
  httpRequestLine(httpTrace(root),V) --> "TRACE",skip(),requestURI(root),skip(),httpVersion(V).
  httpRequestLine(httpOptions(root),V) --> "OPTIONS",skip(),requestURI(root),skip(),httpVersion(V).
  httpRequestLine(httpConnect(root),V) --> "CONNECT",skip(),requestURI(root),skip(),httpVersion(V).
  
  contentMimeType:[string]=>headerAtt.
  contentMimeType(Fn)::append(_,[`.,..Ext],Fn), (Ext,Class,Type) in MimeTypes => 
      contentType(Class,Type,[]).
  contentMimeType(_) => contentType("text","plain",[]).
      
  webClient:[]@=serverProc.
  webClient..{                       -- We have one of these for each connection
    exec(Host,rIp,rPort,i,o) ->
        stdout.outLine("Connection from "<>Host<>" at "<>rIp<>" local port is "<>rPort.show());
        process(i,o).

    process:[inChannel,outChannel]*.
    process(i,_)::i.eof() -> {}.
    process(i,o) -> 
        grabRequest(i,M,A);
        { httpRequestLine(Cmd,V) --> M };
        Hdrs = parseHeaders(combineHeaderLines(A));
        stdout.outLine("Request = "<>Cmd^<>" http version "<>V^);
        (Hd in Hdrs *> stdout.outLine(Hd^));
        handleRequest(Cmd,Hdrs,i,o);
        (keepingAlive(Hdrs)?process(i,o)|stdout.outLine("dropping connection")).

    handleRequest:[httpRequest,list[headerAtt],inChannel,outChannel]*.
    handleRequest(httpGet(req),Hdrs,i,o) ->
        stdout.outLine("GET request: "<>req.show());  -- {__break(req)};
        returnResource(req,Hdrs,i,o).
    handleRequest(httpOptions(ul),_,_,_) -> stdout.outLine("OPTIONS request: "<>ul.show()).

    returnResource:[urlTp,list[headerAtt],inChannel,outChannel]*.
    returnResource(filePath([],X),Hdrs,i,o) -> returnResource(filePath("index.html",X),Hdrs,i,o).
    returnResource(filePath((D::isDirectory(__mergeURL(httpDir,D))),[]),Hdrs,_,o) -> 
        ((expect(E) in Hdrs, "100-continue" in E ? 
            writeResponseHeader(httpContinue,"",[],[],o) | {});
         writeResponseHeader(httpOk,"OK",[contentType("text","plain",[])],
			     listing(__mergeURL(httpDir,D)),o))
        onerror(
         error(_,_) -> o.outLine(errorResponse(httpNotFound,D,fullResponse))
        ).
    returnResource(filePath(L,[]),Hdrs,_,o) -> 
        Fn = __mergeURL(httpDir,L);
        (I = openInFile(Fn,rawEncoding);
         (expect(E) in Hdrs, "100-continue" in E ? 
            writeResponseHeader(httpContinue,"",[],[],o) | {});
         Text = I.inBytes(fsize(Fn));
         writeResponseHeader(httpOk,"OK",[contentMimeType(Fn)],Text,o);
         o.outBytes(Text))
        onerror(
         error(_,_) -> o.outLine(errorResponse(httpNotFound,L,fullResponse))
        ).
    returnResource(FP,_,_,o) -> o.outLine(errorResponse(httpBadRequest,showFilePath(FP),fullResponse)).

    writeResponseHeader:[httpCode,string,list[headerAtt],list[_],outChannel]*.
    writeResponseHeader(Code,Msg,Hdrs,Text,o) ->
        o.outLine("HTTP/1.1 "<>httpErrCode(Code)<>" "<>Msg);
        o.outLine("Date: "<>time2utc(now()).show());
        o.outLine("Server: GoWeb/0.0");
        (H in Hdrs *> writeHeader(H,o));
        o.outLine("Content-Length: "<>listlen(Text).show());
        o.outLine("").
  
    writeHeader:[headerAtt,outChannel]*.
    writeHeader(connection(L),o) -> o.outLine("Connection:"<>mergeLists(L)).
    writeHeader(contentType(Class,Type,_),o) -> o.outLine("Content-Type: "<>Class<>"/"<>Type).

    mergeLists:[list[string]]=>string.
    mergeLists([]) => "".
    mergeLists([L,R,..M]) => L<>[` ,..mergeLists([R,..M])].
    mergeLists([L]) => L.
  }.

  listing:[string]=>string.
  listing(_url) => "listing of "<>_url.show().

  isDirectory:[string]{}.
  isDirectory(Dir) :- ftype(Dir)==directory.

  main([P]) ->
    stdout.outLine("GoWeb starting on port "<>P);
      tcpServer(naturalOf%%P,webClient,rawEncoding).
    
}.
