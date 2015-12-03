/* 
   Character handling and Unicode character predicates
   (c) 2001 F.G. McCabe

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

  $Id: char.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: char.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/ 
#include "config.h"		/* pick up standard configuration header */

#include "go.h"
#include "eval.h"
#include "unicode.h"
#include "hash.h"		/* access the hash functions */
#include "char.h"

ptrI charClass;

/* Standard character dictionary ... */
/* We keep a hash table of char values to avoid keeping 1M characters around */
static hashPo characters;       
static uinteger idHash(void *c);
static int idComp(void *a,void *b);

static long chSizeFun(specialClassPo class,objPo o);
static comparison chCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode chOutFun(specialClassPo class,ioPo out,objPo o);
static retCode chScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo chCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger chHashFun(specialClassPo class,objPo o);

void initCharClass(void)
{
  charClass=newSpecialClass("go.stdlib#char",chSizeFun,chCompFun,chOutFun,
			    chCopyFun,chScanFun,chHashFun);

  characters = NewHash(256,idHash,idComp,NULL);
}

static long chSizeFun(specialClassPo class,objPo o)
{
  return CellCount(sizeof(charRec));
}

static comparison chCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1->class==charClass && o2->class==charClass){
    charPo c1 = (charPo)o1;
    charPo c2 = (charPo)o2;

    if(c1->uni==c2->uni)
      return same;
    else if(c1->uni<c2->uni)
      return smaller;
    else
      return bigger;
  }
  else
    return incomparible;
}

static retCode chOutFun(specialClassPo class,ioPo out,objPo o)
{
  assert(o->class==charClass);

  charPo c = (charPo)o;

  retCode ret = outChar(out,'`');

  if(ret==Ok)
    ret = wStringChr(out,CharVal(c));

  return ret;
}

static retCode chScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  return Ok;
}

static objPo chCopyFun(specialClassPo class,objPo dst,objPo src)
{
  charPo chDst = (charPo)dst;
  charPo chSrc = (charPo)src;

  *chDst = *chSrc;
  return (objPo)(chDst+1);
}

static uinteger chHashFun(specialClassPo class,objPo o)
{
  charPo c = (charPo)o;

  return CharVal(c);
}

/*
 * Allocation within the global heap
 */

static ptrI permChar(const uniChar ch)
{
  charPo new = (charPo)permAllocate(CellCount(sizeof(charRec)));

  new->sign = charClass;
  new->uni = ch;
  return objP(new);
}

/* Locate a character in the dictionary */
ptrI newChar(const uniChar ch)
{
  unsigned long C = (unsigned long)ch;
  lockHash(characters);			/* We need to lock the table */
  ptrI chr = (ptrI)Search((void*)C,characters);

  if(objV(chr)==NULL){			/* A new entry in the table */
    chr = permChar(ch);

    Install((void*)C,(void*)chr,characters); 
  }
  unlockHash(characters);		/* unlock the character table */
  return chr;				/* return the character */
}

/* remove all entries from the character dictionary */
typedef struct {
  globalGcPo G;
  hashPo newDict;
} DInfoRec;

static retCode remChar(void *n,void *r,void *c)
{
  DInfoRec *I = (DInfoRec *)c;
  ptrI S = (ptrI)r;

  Uninstall(n,characters);

  S = scanPtr(I->G,S);

  Install((void*)n,(void*)S,characters);

  return Ok;
}


void restartChars(globalGcPo G)
{
  DInfoRec help = {G,NewHash(256,idHash,idComp,NULL)};
  hashPo currDict = characters;

  characters = help.newDict;

  ProcessTable(remChar,currDict,&help);

  DelHash(currDict);			/* clear down the existing dictionary */
}


/* 
 * Character type predicates
 */

retCode g_isCcChar(processPo P,ptrPo a)         /* Other, Control */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isCcChar",eINSUFARG);
  else{
    if(isCcChar(CharVal((charPo)objV(x))))
      return Ok;
    else
      return Fail;
  }
}

retCode g_isCfChar(processPo P,ptrPo a)         /* Other, Format */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isCfChar",eINSUFARG);
  else{
    if(isCfChar(CharVal((charPo)objV(x))))
      return Ok;
    else
      return Fail;
  }
}

retCode g_isCnChar(processPo P,ptrPo a)         /* Other, Unassigned */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isCnChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isCnChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isCnChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isCoChar(processPo P,ptrPo a)         /* Other, Private */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isCoChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isCoChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isCoChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isCsChar(processPo P,ptrPo a)         /* Other, surrogate */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isCsChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isCsChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isCsChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isLlChar(processPo P,ptrPo a)         /* Letter, lowercase */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isLlChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isLlChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isLlChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isLmChar(processPo P,ptrPo a)         /* Letter, modifier */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isLmChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isLmChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isLmChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isLoChar(processPo P,ptrPo a)         /* Letter, other */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isLoChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isLoChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isLoChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isLtChar(processPo P,ptrPo a)         /* Letter, titlecase */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isLtChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isLtChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isLtChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isLuChar(processPo P,ptrPo a)         /* Letter, uppercase */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isLuChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isLuChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isLuChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isMcChar(processPo P,ptrPo a)         /* Mark, spacing combining */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isMcChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isMcChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isMcChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isMeChar(processPo P,ptrPo a)         /* Mark, enclosing */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isMeChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isMeChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isMeChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isMnChar(processPo P,ptrPo a)         /* Mark, non spacing */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isMnChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isMnChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isMnChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isNdChar(processPo P,ptrPo a)         /* Number, decimal digit */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isNdChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isNdChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isNdChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isNlChar(processPo P,ptrPo a)         /* Number, letter */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isNlChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isNlChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isNlChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isNoChar(processPo P,ptrPo a)         /* Number, other */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isNoChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isNoChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isNoChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isPcChar(processPo P,ptrPo a)         /* Punctuation, connector */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isPcChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isPcChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isPcChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isPdChar(processPo P,ptrPo a)         /* Punctuation, dash */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isPdChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isPdChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isPdChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isPeChar(processPo P,ptrPo a)         /* Punctuation, close */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isPeChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isPeChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isPeChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isPfChar(processPo P,ptrPo a)         /* Punctuation, final quote */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isPfChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isPfChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isPfChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isPiChar(processPo P,ptrPo a)         /* Punctuation, initial quote */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isPiChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isPiChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isPiChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isPoChar(processPo P,ptrPo a)         /* Punctuation, other */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isPoChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isPoChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isPoChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isPsChar(processPo P,ptrPo a)         /* Punctution, open */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isPsChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isPsChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isPsChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isScChar(processPo P,ptrPo a)         /* Symbol, currency */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isScChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isScChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isScChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isSkChar(processPo P,ptrPo a)         /* Symbol, modifier */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isSkChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isSkChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isSkChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isSmChar(processPo P,ptrPo a)         /* Symbol, math */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isSmChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isSmChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isSmChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isSoChar(processPo P,ptrPo a)         /* Symbol, other */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isSoChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isSoChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isSoChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isZlChar(processPo P,ptrPo a)         /* Separator, line */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isZlChar",eINSUFARG);
  else{
    uniChar ch = CharVal(charV(x));

    if(isZlChar(ch))
      return Ok;
    else
      return Fail;
  }
}

retCode g_isZpChar(processPo P,ptrPo a)         /* Separator, paragraph */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isZpChar",eINSUFARG);
  else{
    uniChar ch = CharVal(charV(x));

    if(isZpChar(ch))
      return Ok;
    else
      return Fail;
  }
}

retCode g_isZsChar(processPo P,ptrPo a)         /* Separator, space */
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isZsChar",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__isZsChar",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isZsChar(ch))
	return Ok;
      else
	return Fail;
    }
  }
}

retCode g_isLetterChar(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__isLetterChar",eINSUFARG);
  else{
    uniChar ch = CharVal(charV(x));

    if(isLetterChar(ch))
      return Ok;
    else
      return Fail;
  }
}

retCode g_digitCode(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__digitCode",eINSUFARG);
  else{
    if(!IsChar(x))
      return liberror(P,"__digitCode",eCHRNEEDD);
    else{
      uniChar ch = CharVal(charV(x));

      if(isNdChar(ch)){
	ptrI ans = allocateInteger(&P->proc.heap,digitValue(ch));
        return equal(P,&a[2],&ans);
      }
      else
	return Fail;
    }
  }
}

retCode g_charOf(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__charOf",eINSUFARG);
  else{
    objPo X = objV(x);

    if(!isInteger(X))
      return Fail;
    else{
      ptrI ch = newChar(integerVal((integerPo)X));

      return equal(P,&a[2],&ch);
    }
  }
}

retCode g_charCode(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"__charCode",eINSUFARG);
  else{
    if(!IsChar(x))
      return Fail;
    else{
      ptrI ch = allocateInteger(&P->proc.heap,CharVal(charV(x)));

      return equal(P,&a[2],&ch);
    }
  }
}

retCode g_succChar(processPo P,ptrPo a)
{
  ptrI c = deRefI(&a[1]);
  ptrI x = deRefI(&a[2]);

  if(isvar(c)||isvar(x))
    return liberror(P,"__succChar",eINSUFARG);
  else{
    objPo X = objV(x);
    if(!IsChar(c)||!isInteger(X))
      return Fail;
    else{
      ptrI ch = newChar(CharVal(charV(c))+integerVal((integerPo)X));

      return equal(P,&a[3],&ch);
    }
  }
}

static uinteger idHash(void *c)
{
  return (uinteger)((long)c);
}

static int idComp(void *a,void *b)
{
  if(a==b)
    return 0;
  else
    return -1;
}

