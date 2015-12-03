/* 
   Class management module
   (c) 2000-2007 F.G. McCabe

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

#include "config.h"		/* pick up standard configuration header */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
 
#include <assert.h>
#include <unistd.h>
#include <dirent.h>
#include <pwd.h>
#include <sys/types.h>

#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>

#include <signal.h>

#include "go.h"
#include "process.h"
#include "dict.h"
#include "symbols.h"
#include "clock.h"
#include "fileio.h"
#include "term.h"
#include "str.h"
#include "char.h"
#include "encoded.h"             /* pick up the term encoding definitions */
#include "hashtable.h"
#include "process.h"

#ifndef MAX_FILE_LEN
#define MAX_FILE_LEN 2048
#endif

#ifndef MAX_PATH_LEN
#define MAX_PATH_LEN 4096
#endif

/* Dictionary of known classes ... */
static hashPo classes,specialClasses;

/* Standard constructor templates */
ptrI classClass;
ptrI specialClass;
ptrI programClass;

/* Standard classes */
ptrI emptyList;				/* the empty list */
ptrI nilClass;				/* The empty list class */
ptrI listClass;				/* the non-empty list */
ptrI commaClass;			/* the tupling constructor */

ptrI thingClass;			/* root type/class */
ptrI thingProg;				/* The thing program label */

ptrI integerClass;			/* The integers */
ptrI floatClass;			/* F.P. numbers */
ptrI symbolClass;			/* Standard symbol class */

ptrI trueClass,falseClass;		/* True and False */

ptrI threadClass;			/* The class of process identifiers */

static hashPo loadedPackages;		/* record the packages we have loaded */

typedef struct{
  uniChar packageName[MAX_SYMB_LEN];	/* A package name */
  uniChar version[MAX_SYMB_LEN];
} PackageRec, *packagePo;

static long clSizeFun(specialClassPo class,objPo o);
static comparison clCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode clOutFun(specialClassPo class,ioPo out,objPo o);
static retCode clScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static uinteger clHashFun(specialClassPo class,objPo o);
static objPo clCopyFun(specialClassPo class,objPo dst,objPo src);

static long spSizeFun(specialClassPo class,objPo o);
static comparison spCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode spOutFun(specialClassPo class,ioPo out,objPo o);
static retCode spScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo spCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger spHashFun(specialClassPo class,objPo o);

void initClass()
{
  classes = NewHash(256,(hashFun)uniHash,(compFun)uniCmp,NULL);
  specialClasses = NewHash(256,(hashFun)uniHash,(compFun)uniCmp,NULL);
  loadedPackages = NewHash(256,(hashFun)uniHash,(compFun)uniCmp,NULL);

  // The first class record has to be created carefully
  specialClass = newSpecialClass("#special",spSizeFun,spCompFun,
				 spOutFun,spCopyFun,
				 spScanFun,spHashFun);
  specialClassPo sClass = (specialClassPo)objV(specialClass);

  classClass = newSpecialClass("#class",clSizeFun,
			       clCompFun,clOutFun,clCopyFun,
			       clScanFun,clHashFun);
  specialClassPo class = (specialClassPo)objV(classClass);

  class->class = specialClass;		/* class is a special class */
  sClass->class = specialClass;		/* specialClass is a special class! */
}

// Return the size of a class object
static long clSizeFun(specialClassPo class,objPo o)
{
  assert(o!=NULL);
  assert(o->class==classClass);

  clssPo cl = (clssPo)o;
  return CellCount(sizeof(clssRec)+sizeof(uniChar)*(uniStrLen(cl->name)+1));
}

static comparison clCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1==o2)
    return same;
  else
    return incomparible;
}

static retCode clOutFun(specialClassPo class,ioPo out,objPo o)
{
  clssPo cl = (clssPo)o;

  return outMsg(out,"%U/%d",cl->name,cl->arity);
}

static retCode clScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  return Ok;
}

static objPo clCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = clSizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static uinteger clHashFun(specialClassPo class,objPo o)
{
  clssPo cl = (clssPo)o;

  return cl->hash;
}

static long spSizeFun(specialClassPo class,objPo o)
{
  assert(o->class==specialClass);

  specialClassPo cl = (specialClassPo)o;

  return CellCount(sizeof(specialClassRec)+sizeof(uniChar)*(uniStrLen(cl->name)+1));
}

static comparison spCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1==o2)
    return same;
  else
    return incomparible;
}

static retCode spOutFun(specialClassPo class,ioPo out,objPo o)
{
  specialClassPo cl = (specialClassPo)o;

  return outMsg(out,"%U",cl->name);
}

static retCode spScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  return Ok;
}

static objPo spCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = spSizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static uinteger spHashFun(specialClassPo class,objPo o)
{
  specialClassPo cl = (specialClassPo)o;

  return uniHash(cl->name);
}

// This is called to set up the standard classes in the system
void standardClasses(void)
{
  kvoid = newEnumSym("go.stdlib#void");

  thingClass = newClassDf("go.stdlib#thing",0);
  thingProg = newProgLbl("go.stdlib#thing",3);

  emptyList = newEnumSym("go.stdlib#[]");
  nilClass = newClassDf("go.stdlib#[]",0);
  listClass = newClassDf("go.stdlib#,..",2);
  commaClass = newClassDf("go.stdlib#,",2);

  suspClass=newClassDf("#suspension",3);
  varClass=newClassDf("#var",1);

  trueClass = newEnumSym("go.stdlib#true");
  falseClass = newEnumSym("go.stdlib#false");

  initCodeClass();
  initCharClass();
  initSymbolClass();
  initDynamicClass();
  initArithClasses();
  initHashClass();
  initThreadClass();
}

ptrI newClassDef(const uniChar *name,long arity)
{
   ptrI def = (ptrI)Search((void*)name,classes); 

  if(objV(def)==NULL){
    long symlen = uniStrLen(name);
    long len = CellCount(sizeof(clssRec)+sizeof(uniChar)*(symlen+1));
    clssPo new = (clssPo)permAllocate(len);

    new->class = classClass;
    new->hash = uniHash(name);
    new->arity = arity;

    memcpy(new->name,name,(symlen+1)*sizeof(uniChar));

    def = objP(new);

    Install(className(new),(void*)def,classes);
    return def;
  }
  else{
    assert(classArity((clssPo)objV(def))==arity);
    return def;
  }
}

void installClass(clssPo class)
{
  uniChar *name = className(class);
  ptrI cl = (ptrI)Search(name,classes);

  if(objV(cl)==NULL)
    Install(name,(void*)objP(class),classes);
}

ptrI classPresent(uniChar *name)
{
  return (ptrI)Search(name,classes);
}

ptrI newClassDf(const char *name,long arity)
{
  int slen = strlen(name);
  uniChar buff[slen+1];

  _uni((unsigned char*)name,buff,slen+1);

  return newClassDef(buff,arity);
}

ptrI newEnumSym(const char *fun)
{
  ptrI eClass = newClassDf(fun,0);

  return objP(allocateObject(&globalHeap,eClass));
}

ptrI newEnumSymbol(const uniChar *fun)
{
  ptrI eClass = newClassDef(fun,0);

  return objP(allocateObject(&globalHeap,eClass));
}

ptrI newSpecialClass(const char *name,
		     classSizeFun sizeFun, classCompFun compFun,
		     classOutFun outFun, classCpyFun copyFun,
		     classScanFun scanFun, classHashFun hashFun)
{
  int slen = strlen(name);
  uniChar buff[slen+1];

  _uni((unsigned char*)name,buff,slen+1);

  specialClassPo new = (specialClassPo)malloc(sizeof(specialClassRec)+(uniStrLen(buff)+1)*sizeof(uniChar));

  new->class = specialClass;
  new->sizeFun = sizeFun;
  new->compFun = compFun;
  new->outFun = outFun;
  new->copyFun = copyFun;
  new->scanFun = scanFun;
  new->hashFun = hashFun;

  memcpy(&new->name,buff,(uniStrLen(buff)+1)*sizeof(uniChar));

  Install(&new->name,(void*)objP(new),specialClasses);

  if(name[0]!='#')
    new->program = defineSpecialProg(name);
  else
    new->program = 0;

  return objP(new);
}

/* remove all entries from the class directory */
typedef struct {
  globalGcPo G;
  hashPo newDict;
} DInfoRec;

static retCode remClass(void *n,void *r,void *c)
{
  DInfoRec *I = (DInfoRec *)c;
  ptrI S = (ptrI)r;

  Uninstall(n,classes);

  objPo o = objV(S);

  /* This fragment allows code to be garbage collected - except for code loaded as part of a package */
  if(oldGeneration(o))
    Install(className((clssPo)objV(S)),(void*)S,I->newDict); /* put symbol directly into the new dictionary */
  else
    scanPtr(I->G,S); /* We keep defined programs FIXME */

  return Ok;
}

static retCode markSpecialClass(void *n,void *r,void *c)
{
  DInfoRec *I = (DInfoRec *)c;
  specialClassPo spClass = (specialClassPo)objV(r);

  spClass->program = scanPtr(I->G,spClass->program); /* We keep special programs */

  return Ok;
}

void markStandardClasses(globalGcPo G)
{
  DInfoRec help = {G,NewHash(256,(hashFun)uniHash,(compFun)uniCmp, NULL)};
  hashPo currClassTable = classes;

  classes = help.newDict;
  ProcessTable(remClass,currClassTable,&help);

  thingClass = scanPtr(G,thingClass);

  emptyList = scanPtr(G,emptyList);
  nilClass = scanPtr(G,nilClass);
  listClass = scanPtr(G,listClass);
  commaClass = scanPtr(G,commaClass);
  errorClass = scanPtr(G,errorClass);

  /*
    integerClass = scanPtr(G,integerClass);
    floatClass = scanPtr(G,floatClass);
  charClass = scanPtr(G,charClass);
  symbolClass = scanPtr(G,symbolClass);

  filePtrClass = scanPtr(G,filePtrClass);
  threadClass = scanPtr(G,threadClass);
  procClass = scanPtr(G,procClass);

  dynamicClass = scanPtr(G,dynamicClass);
  */

  suspClass = scanPtr(G,suspClass);
  varClass = scanPtr(G,varClass);
  trueClass = scanPtr(G,trueClass);
  falseClass = scanPtr(G,falseClass);

  ProcessTable(markSpecialClass,specialClasses,&help);
}


static uniChar *computeClassFileName(uniChar *path,long pathLen,
                                     uniChar *className,uniChar *version,
                                     uniChar *fn,long fLen);


retCode classLoader(heapPo H,uniChar *path,ptrI request,ptrI rqV,
		    ptrPo loaded,uniChar *errorMsg,long msgSize)
{
  packagePo ldFlag = (packagePo)Search(SymVal(symbV(request)),loadedPackages);

  if(ldFlag!=NULL){
    if(rqV!=emptySymbol)
      if(uniCmp(SymVal(symbV(rqV)),ldFlag->version)==0)
        return Ok;
      else{
        outMsg(logFile,"invalid version of package already loaded: %w:%U,"
	       "version %w expected\n",&request,ldFlag->version,&rqV);
        return Error;
      }
    else
      return Ok;
  }
  else{
    uniChar fbuffer[MAX_FILE_LEN];
    uniChar *fn = computeClassFileName(path,uniStrLen(path),SymVal(symbV(request)),SymVal(symbV(rqV)),fbuffer,NumberOf(fbuffer));
    ioPo file = fn!=NULL?openInFile(fn,rawEncoding):NULL;
    uniChar ch;

#ifdef EXECTRACE
    if(debugging)
      outMsg(logFile,"loading package %w from file %U\n",&request,fn);
#endif

    if(file!=NULL){
      retCode ret = Ok;
      ptrI scratch = kvoid;
      ptrI package = kvoid;
      ptrI version = kvoid;		/* version of the loaded package */
      ptrI imports = kvoid;
      ptrI defined = kvoid;
      rootPo root = gcAddRoot(H,&scratch);

      gcAddRoot(H,&package);
      gcAddRoot(H,&request);
      gcAddRoot(H,&version);
      gcAddRoot(H,&imports);
      gcAddRoot(H,&defined);

      if((ch=inCh(file))=='#'){			/* look for standard #!/.... header */
        if((ch=inCh(file))=='!'){
          while((ch=inCh(file))!=uniEOF && ch!=NEW_LINE)
            ;			                /* consume the interpreter statement */
        }
        else{
          unGetChar(file,ch);
          unGetChar(file,'#');
        }
      }
      else
        unGetChar(file,ch);
      
      if(fileStatus(file)==Ok){
	ret = decodeTerm(file,&globalHeap,H,&package,errorMsg,msgSize);
        
	if(package!=request){
	  outMsg(logFile,"loaded package: %w not what was expected %w\n",
		 &package,&request);
	  return Error;
	}
        
	ret = decodeTerm(file,&globalHeap,H,&version,errorMsg,msgSize);
        
	//outMsg(logFile,"package is %w:%w\n",&package,&version);
        
	if(version!=rqV && rqV!=emptySymbol && version!=universal){
	  outMsg(logFile,"invalid version of package: %w:%w,"
		 "version %w expected\n",&request,&version,&rqV);
	  return Error;
	}

	packagePo pkgInfo = malloc(sizeof(PackageRec));

	uniCpy(pkgInfo->packageName,NumberOf(pkgInfo->packageName),SymVal(symbV(package)));
	uniCpy(pkgInfo->version,NumberOf(pkgInfo->version),SymVal(symbV(version)));

	Install(pkgInfo->packageName,pkgInfo,loadedPackages);
        
	if(ret==Ok)
	  ret = skipEncoded(file,errorMsg,msgSize);
        
	if(ret==Ok)
	  ret = skipEncoded(file,errorMsg,msgSize);
        
	if(ret==Ok){
	  ret = decodeTerm(file,&globalHeap,H,&imports,errorMsg,msgSize);
	  //	  outMsg(logFile,"Imported packages: %w\n",&imports);
	}

	if(ret==Ok){
	  ret = decodeTerm(file,&globalHeap,H,&defined,errorMsg,msgSize); /* Locally defined programs */

	  //	  setProperty(H,package,kdefined,defined);
	}
        
	if(ret==Ok){
	  if(IsList(imports)){
	    ptrI imps = imports;
	    gcAddRoot(H,&imps);

	    while(IsList(imps)){
	      ptrI entry = deRefI(listHead(objV(imps)));

	      if(HasClass(entry,commaClass)){
		ptrI import = deRefI(nthArg(objV(entry),0));
		ptrI vers = deRefI(nthArg(objV(entry),1));
		rootPo subRoot = gcAddRoot(H,&import);

		assert(symbolPresent(SymVal(symbV(import)))==import);
		
		gcAddRoot(H,&vers);
		gcAddRoot(H,&import);
            
		ret = classLoader(H,path,import,vers,loaded,errorMsg,msgSize);
	      
		gcRemoveRoot(H,subRoot);

		switch(ret){
		case Ok:
		  break;
		default:
		case Error:
		  outMsg(logFile,"Failed to load package %U, "
			 "[version: %U] requested by %U\n",
			 SymVal(symbV(import)),SymVal(symbV(vers)),
			 SymVal(symbV(request)));
		  break;
		case Space:
		  outMsg(logFile,"Not enough heap space to load package %U, "
			 "[version: %U] requested by %U\n",
			 SymVal(symbV(import)),SymVal(symbV(vers)),
			 SymVal(symbV(request)));
		  break;
		}
	      }
	      else
		outMsg(logFile,"invalid version info import package "
		       "spec %w, requested by %w\n",&imports,&request);
	      imps = deRefI(listTail(objV(imps)));
	    }
	  }
	  else if(!identical(imports,emptyList))
	    outMsg(logFile,"invalid import package spec %w, "
		   "requested by %w\n",&imports,&request);
	}

	while(ret==Ok){
	  ret = decodeTerm(file,&globalHeap,H,&scratch,errorMsg,msgSize);
          
	  if(ret==Ok){
	    if(HasClass(scratch,commaClass)){
	      ptrI prog = deRefI(nthArg(objV(scratch),1));
	      ptrI symb = deRefI(nthArg(objV(scratch),0));
	      if(IsCode(prog) && IsProgLbl(symb)){
		defineProg(symb,prog);

#ifdef EXECTRACE
		if(debugging)
		  outMsg(logFile,"program %w loaded\n",&symb);
#endif
	      }
	      else
		outMsg(logFile,"code expected, not: %#,4w in code file",&scratch);
	    }
	    else
	      outMsg(logFile,"invalid entry: %#,4w in code file",&scratch);
	  }
	}
      }

      if(ret!=Space)
	*loaded = permLsPair(H,request,*loaded);
      
      gcRemoveRoot(H,root);
      closeFile(file);

#ifdef EXECTRACE
      if(debugging)
        outMsg(logFile,"package %w loaded\n",&package);
#endif
      
      if(ret==Eof)
        return Ok;
      else
        return ret;
    }
    else{ 
      strMsg(errorMsg,msgSize,"package %U not found",SymVal(symbV(request)));
      return Eof;
    }
  }
}

retCode g_classload(processPo P,ptrPo a)
{
  ptrI pname = deRefI(&a[2]);

  if(isvar(pname))
    return liberror(P,"__ensure_loaded",eINSUFARG);
  else if(!IsSymb(pname))
    return liberror(P,"__ensure_loaded",eINVAL);
  else{
    if(isLoaded(pname))
      return Ok;
    else{
      uniChar dirPath[MAX_PATH_LEN];
      ptrI A1 = deRefI(&a[1]);

      if(isvar(A1)||isGroundString(&A1)!=Ok)
        return liberror(P,"__ensure_loaded",eSTRNEEDD);
      else{
        ptrI loaded = emptyList;
	heapPo H = &P->proc.heap;

	gcAddRoot(H,&loaded);
    
        String2Uni(&A1,dirPath,NumberOf(dirPath));

	switchProcessState(P,wait_io);	/* Potentially nec. to wait */

        retCode ret = classLoader(H,dirPath,pname,deRefI(&a[3]),&loaded,
				  P->proc.errorMsg,NumberOf(P->proc.errorMsg));
	setProcessRunnable(P);

        switch(ret){
        case Error:{
          uniChar msg[MAX_MSG_LEN];
          
          strMsg(msg,NumberOf(msg),"__ensure_loaded: %#w in %#w",&a[2],&a[1]);
          return raiseError(P,msg, eNOTFND);
        }
        case Eof:{
          uniChar msg[MAX_MSG_LEN];
          
          strMsg(msg,NumberOf(msg),"__ensure_loaded: %#w in %#w",&a[2],&a[1]);
          return raiseError(P,msg, eNOFILE);
        }
        case Ok:
          return equal(P,&a[4],&loaded);
        case Fail:
          return Fail;
	case Space:
	  outMsg(logFile,"Out of heap space, increase and try again\n%_");
          return liberror(P,"__ensure_loaded",eSPACE);
        default:
          return liberror(P,"__ensure_loaded",eINVAL);
        }
      }
    }
  }
}

logical isLoaded(ptrI pkg)
{
  if((packagePo)Search(SymVal(symbV(pkg)),loadedPackages)!=NULL)
    return True;
  else
    return False;
}

static retCode showPkg(void *n,void *r,void *c)
{
  return outMsg(logFile,"%U loaded\n",n);
}

void showPackages(void)
{
  ProcessTable(showPkg,loadedPackages,NULL);
  flushOut();
}

static logical trimDirs(uniChar *path,uniChar *request,uniChar *buffer,long len,
			uniChar *suffix);

// Attempt to compute a file name to load a package from, based on a list of paths to try
static uniChar *computeClassFileName(uniChar *path,long pathLen,uniChar *className,uniChar *version,uniChar *fn,long fLen)
{
  uniChar *p = path;
  long len = pathLen;
  uniChar bLen = fLen-strlen(".goc")-1;
  uniChar buffer[MAX_FILE_LEN];
  uniChar pthBuffer[MAX_FILE_LEN];

  while(p!=NULL){
    uniChar *next = uniSearch(p,len,':');
    long i=0;
    uniChar *c = className;

    if(next!=NULL){
      for(;p<next && i<bLen;i++)
        pthBuffer[i]=buffer[i]=*p++;

      pthBuffer[i]='\0';

      if(buffer[i-1]!='/'){
        pthBuffer[i]=buffer[i]='/';
	i++;
	pthBuffer[i]='\0';
      }
      p=next+1;
    }
    else{
      for(i=0;*p!='\0'&&i<bLen;i++)
        pthBuffer[i]=buffer[i]=*p++;
      pthBuffer[i]='\0';
      if(buffer[i]!='/')
        buffer[i++]='/';
      else
        i++;
      p=NULL;
    }

    if(buffer[i-1]!='/')
      buffer[i++]='/';			/* Join the path segment with a / */
      
    for(;*c!='\0'&&i<bLen;i++,c++){
      if(*c=='.')
        buffer[i]='/';
      else
        buffer[i]=*c;
    }

    if(uniStrLen(version)>0)
      strMsg(&buffer[i],fLen-i,".%U.goc",version);
    else
      strMsg(&buffer[i],fLen-i,".goc");	/* we drop through if no version */

    if(filePresent(buffer)){
      uniCpy(fn,fLen,buffer);
      return fn;
    }
    else{
      uniChar fBuffer[MAX_FILE_LEN];
      uniChar suffix[MAX_FILE_LEN];

      if(uniStrLen(version)>0)
	strMsg(suffix,NumberOf(suffix),".%U.goc",version);
      else
	strMsg(suffix,NumberOf(suffix),".goc");
      
      if(trimDirs(pthBuffer,className,fBuffer,NumberOf(fBuffer),suffix)){
	if(filePresent(fBuffer)){
	  uniCpy(fn,fLen,fBuffer);
	  return fn;
	}
      }
    }
  }
  return NULL;
}

// map the path foo/bar/gamma and the request bar.gamma.delta
// to a 'real' path of foo/bar/gamma/delta

static void reverse(uniChar *str);

static logical trimDirs(uniChar *path,uniChar *request,uniChar *buffer,long len,
			uniChar *suffix)
{
  uniChar pathBrks[MAX_FILE_LEN];
  uniChar reqBrks[MAX_FILE_LEN];
  long pCount = 0,rCount=0;

  long i=0,mx = uniStrLen(path);
  uniChar *pth = &pathBrks[NumberOf(pathBrks)-1];
  *--pth = '\0';			/* We are going to reverse this */
  
  for(i=0;i<mx;i++){			/* We split the path into a sequence */
    if(path[i]!='/')			/* of segments */
      *--pth=path[i];
    else{
      reverse(pth);
      if(uniStrLen(pth)!=0){
	*--pth='\0';
	pCount++;
      }
    }
  }
  reverse(pth);
  if(uniStrLen(pth)!=0)
    pCount++;
  else
    pth++;				/* remove the empty chunk */

  mx = uniStrLen(request);

  for(i=0;i<mx;i++){
    if(request[i]=='.'){
      reqBrks[i]='\0';
      rCount++;
    }
    else
      reqBrks[i]=request[i];
  }
  reqBrks[i]='\0';

  if(pCount>0 && rCount>0){	/* look for an overlapping segments */
    uniChar *p = pth;
    uniChar *r = reqBrks;
    for(i=0;i<rCount;i++){
      long j;
      for(j=0;j<=i;j++){
	if(uniCmp(p,r)!=0)
	  goto exLoop;
	p += uniStrLen(p)+1;
	r += uniStrLen(r)+1;		/* step on to the next string */
      }
    }
    exLoop:
    if(p!=pth){				/* We had a sucessful match */
      long preSeg = &pathBrks[NumberOf(pathBrks)]-p;

      uniNCpy(buffer,len,path,preSeg);
      uniChar *r = reqBrks;
      for(i=0;i<rCount;i++){
	uniAppend(buffer,len,r);
	uniTack(buffer,len,"/");
	r += uniStrLen(r)+1;		/* step on to the next string */
      }
      uniAppend(buffer,len,r);
      uniAppend(buffer,len,suffix);

      return True;
    }
  }
  return False;
}

static void reverse(uniChar *str)
{
  long mx = uniStrLen(str);
  long i;
  for(i=0;i<mx/2;i++){
    uniChar ch = str[i];
    str[i]=str[mx-i-1];
    str[mx-i-1]=ch;
  }
}
