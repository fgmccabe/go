/*
  Arithmetic functions for the Go! system
  (c) 2000 F.G. McCabe

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

  $Id: arith.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: arith.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system
 */

#include "config.h"		/* pick up standard configuration header */
#include <math.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>		/* system error numbers */
#include <ctype.h>
#include <string.h>
#include "go.h"
#include "process.h"

static long inSizeFun(specialClassPo class,objPo o);
static comparison nmCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode inOutFun(specialClassPo class,ioPo out,objPo o);
static retCode inScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static uinteger inHashFun(specialClassPo class,objPo o);
static objPo inCopyFun(specialClassPo class,objPo dst,objPo src);

static long ftSizeFun(specialClassPo class,objPo o);
static retCode ftOutFun(specialClassPo class,ioPo out,objPo o);
static retCode ftScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static uinteger ftHashFun(specialClassPo class,objPo o);
static objPo ftCopyFun(specialClassPo class,objPo dst,objPo src);

void initArithClasses(void)
{
  integerClass=newSpecialClass("go.stdlib#integer",inSizeFun,nmCompFun,
			       inOutFun,inCopyFun,inScanFun,inHashFun);
  floatClass=newSpecialClass("go.stdlib#float",ftSizeFun,nmCompFun,
			     ftOutFun,ftCopyFun,ftScanFun,ftHashFun);
}

static long inSizeFun(specialClassPo class,objPo o)
{
  return CellCount(sizeof(integerRec));
}

static comparison nmCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1->class==integerClass){
    integer i1 = integerVal((integerPo)o1);

    if(o2->class==integerClass){
      integer i2 = integerVal((integerPo)o2);


      if(i1==i2)
	return same;
      else if(i1<i2)
	return smaller;
      else
	return bigger;
    }
    else if(o2->class==floatClass){
      number f2 = floatVal((floatPo)o2);

      if(i1==f2)
	return same;
      else if(i1<f2)
	return smaller;
      else
	return bigger;
    }
    else
      return incomparible;
  }
  else if(o1->class==floatClass){
    number f1 = floatVal((floatPo)o1);

    if(o2->class==integerClass){
      integer i2 = integerVal((integerPo)o2);

      if(f1==i2)
	return same;
      else if(f1<i2)
	return smaller;
      else
	return bigger;
    }
    else if(o2->class==floatClass){
      number f2 = floatVal((floatPo)o2);

      if(f1==f2)
	return same;
      else if(f1<f2)
	return smaller;
      else
	return bigger;
    }
    else
      return incomparible;
  }
  else
    return incomparible;
}

static retCode inOutFun(specialClassPo class,ioPo out,objPo o)
{
  integer i = integerVal((integerPo)o);

  return outInteger(out,i,10,0,0,' ',False,"",False);
}

static retCode inScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  return Ok;
}

static objPo inCopyFun(specialClassPo class,objPo dst,objPo src)
{
  integerPo iDst = (integerPo)dst;
  integerPo iSrc = (integerPo)src;

  *iDst = *iSrc;
  return (objPo)(iDst+1);
}

static uinteger inHashFun(specialClassPo class,objPo o)
{
  return abs(((integerPo)o)->i);
}

static long ftSizeFun(specialClassPo class,objPo o)
{
  return CellCount(sizeof(floatRec));
}

static retCode ftOutFun(specialClassPo class,ioPo out,objPo o)
{
  number f = floatVal((floatPo)o);

  return outFloat(out,f);
}

static retCode ftScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  return Ok;
}

static objPo ftCopyFun(specialClassPo class,objPo dst,objPo src)
{
  floatPo fDst = (floatPo)dst;
  floatPo fSrc = (floatPo)src;

  *fDst = *fSrc;
  return (objPo)(fDst+1);
}

static uinteger ftHashFun(specialClassPo class,objPo o)
{
  return 0;
}

/* Simple arithmetic predicates */
retCode g_integral(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  
  if(isvar(x))
    return liberror(P,"integral",eINSUFARG);
  else if(isInteger(objV(x)))
    return Ok;
  else
    return Fail;
}

/* Bitwise arithmetic operators */
retCode g_band(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if(isvar(x) || isvar(y))
    return liberror(P,"band",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if(!isInteger(A1)||!isInteger(A2))
      return liberror(P,"band",eINTNEEDD);
    else if(isvar(z)){
      ptrI R = allocateInteger(&P->proc.heap,integerVal((integerPo)A1)&integerVal((integerPo)A2));

      bindVar(P,deRef(&a[3]),R);
      return Ok;
    }
    else if((integerVal((integerPo)A1)&integerVal((integerPo)A2))==integerVal(intV(z)))
      return Ok;
    else
      return Fail;
  }
}

retCode g_bor(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if(isvar(x) || isvar(y))
    return liberror(P,"bor",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if(!isInteger(A1)||!isInteger(A2))
      return liberror(P,"bor",eINTNEEDD);
    else if(isvar(z)){
      ptrI R = allocateInteger(&P->proc.heap,integerVal((integerPo)A1)|integerVal((integerPo)A2));

      bindVar(P,deRef(&a[3]),R);
      return Ok;
    }
    else if((integerVal((integerPo)A1)|integerVal((integerPo)A2))==integerVal(intV(z)))
      return Ok;
    else
      return Fail;
  }
}

retCode g_bxor(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if(isvar(x) || isvar(y))
    return liberror(P,"bxor",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if(!isInteger(A1)||!isInteger(A2))
      return liberror(P,"bxor",eINTNEEDD);
    else if(isvar(z)){
      ptrI R = allocateInteger(&P->proc.heap,integerVal((integerPo)A1)^integerVal((integerPo)A2));

      bindVar(P,deRef(&a[3]),R);
      return Ok;
    }
    else if((integerVal((integerPo)A1)^integerVal((integerPo)A2))==integerVal(intV(y)))
      return Ok;
    else
      return Fail;
  }
}

retCode g_bleft(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if(isvar(x) || isvar(y))
    return liberror(P,"bleft",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if(!isInteger(A1)||!isInteger(A2))
      return liberror(P,"bleft",eINTNEEDD);
    else if(isvar(z)){
      ptrI R =allocateInteger(&P->proc.heap,integerVal((integerPo)A1)<<integerVal((integerPo)A2));

      bindVar(P,deRef(&a[3]),R);

      return Ok;
    }
    else if((integerVal((integerPo)A1)<<integerVal((integerPo)A2))==integerVal(intV(y)))
      return Ok;
    else
      return Fail;
  }
}

retCode g_bright(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if(isvar(x) || isvar(y))
    return liberror(P,"bright",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if(!isInteger(A1)||!isInteger(A2))
      return liberror(P,"bright",eINTNEEDD);
    else if(isvar(z)){
      ptrI R =allocateInteger(&P->proc.heap,integerVal((integerPo)A1)>>integerVal((integerPo)A2));

      bindVar(P,deRef(&a[3]),R);
      return Ok;
    }
    else if((integerVal((integerPo)A1)>>integerVal((integerPo)A2))==integerVal(intV(y)))
      return Ok;
    else
      return Fail;
  }
}

retCode g_bnot(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(isvar(x))
    return liberror(P,"bnot",eINSUFARG);
  else{
    objPo A1 = objV(x);

    if(!isInteger(A1))
      return liberror(P,"bnot",eINTNEEDD);
    else if(isvar(y)){
      ptrI R = allocateInteger(&P->proc.heap,~integerVal((integerPo)A1));

      bindVar(P,deRef(&a[2]),R);
      return Ok;
    }
    else if(~integerVal((integerPo)A1)==integerVal(intV(y)))
      return Ok;
    else
      return Fail;
  }
}

/* Modulo arithmetic */
retCode g_iplus(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI m = deRefI(&a[3]);
  ptrI z = deRefI(&a[4]);

  if(isvar(x) || isvar(y) || isvar(m))
    return liberror(P,"iplus",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo A2 = objV(y);
    objPo M = objV(m);

    if(!isInteger(A1)||!isInteger(A2)||!isInteger(M))
      return liberror(P,"iplus",eINTNEEDD);
    else{
      integer rslt = integerVal((integerPo)A1)+integerVal((integerPo)A2);
      integer mod = integerVal((integerPo)M);
      
      if(mod>0)
        rslt = rslt%mod;                /* Modulo arithmetic */
        
      if(isvar(z)){
        ptrI R = allocateInteger(&P->proc.heap,rslt);

        bindVar(P,deRef(&a[4]),R);
        return Ok;
      }
      else if(integerVal(intV(z))==rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g_iminus(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI m = deRefI(&a[3]);
  ptrI z = deRefI(&a[4]);

  if(isvar(x) || isvar(y) || isvar(m))
    return liberror(P,"iminus",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo A2 = objV(y);
    objPo M = objV(m);

    if(!isInteger(A1)||!isInteger(A2)||!isInteger(M))
      return liberror(P,"iminus",eINTNEEDD);
    else{
      integer rslt = integerVal((integerPo)A1)-integerVal((integerPo)A2);
      integer mod = integerVal((integerPo)M);
      
      if(mod>0)
        rslt = rslt%mod;                /* Modulo arithmetic */
        
      if(isvar(z)){
        ptrI R = allocateInteger(&P->proc.heap,rslt);

        bindVar(P,deRef(&a[4]),R);
        return Ok;
      }
      else if(integerVal(intV(z))==rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g_itimes(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI m = deRefI(&a[3]);
  ptrI z = deRefI(&a[4]);

  if(isvar(x) || isvar(y) || isvar(m))
    return liberror(P,"itimes",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo A2 = objV(y);
    objPo M = objV(m);

    if(!isInteger(A1)||!isInteger(A2)||!isInteger(M))
      return liberror(P,"itimes",eINTNEEDD);
    else{
      integer rslt = integerVal((integerPo)A1)*integerVal((integerPo)A2);
      integer mod = integerVal((integerPo)M);
      
      if(mod>0)
        rslt = rslt%mod;                /* Modulo arithmetic */
        
      if(isvar(z)){
        ptrI R = allocateInteger(&P->proc.heap,rslt);

        bindVar(P,deRef(&a[4]),R);
        return Ok;
      }
      else if(integerVal(intV(z))==rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g_idiv(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI m = deRefI(&a[3]);
  ptrI z = deRefI(&a[4]);

  if(isvar(x) || isvar(y) || isvar(m))
    return liberror(P,"idiv",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo A2 = objV(y);
    objPo M = objV(m);

    if(!isInteger(A1)||!isInteger(A2)||!isInteger(M))
      return liberror(P,"idiv",eINTNEEDD);
    else if(integerVal((integerPo)A2)==0)
      return liberror(P,"idiv",eDIVZERO);
    else{
      integer rslt = integerVal((integerPo)A1)/integerVal((integerPo)A2);
      integer mod = integerVal((integerPo)M);
      
      if(mod>0)
        rslt = rslt%mod;                /* Modulo arithmetic */
        
      if(isvar(z)){
        ptrI R = allocateInteger(&P->proc.heap,rslt);

        bindVar(P,deRef(&a[4]),R);
        return Ok;
      }
      else if(integerVal(intV(z))==rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g_imod(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI m = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if(isvar(x) || isvar(m))
    return liberror(P,"imod",eINSUFARG);
  else{
    objPo A1 = objV(x);
    objPo M = objV(m);

    if(!isInteger(A1)||!isInteger(M))
      return liberror(P,"imod",eINTNEEDD);
    else{
      integer rslt = integerVal((integerPo)A1)%integerVal((integerPo)M);      
        
      if(isvar(z)){
        ptrI R = allocateInteger(&P->proc.heap,rslt);

        bindVar(P,deRef(&a[3]),R);
        return Ok;
      }
      else if(integerVal(intV(z))==rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

/* Term inequalities */
retCode g_less(processPo P,ptrPo a)
{
  switch(compTerm(&a[1],&a[2])){
    case smaller:
      return Ok;
    case same:
    case bigger:
      return Fail;
    default:
      return liberror(P,"<",eINVAL);
  }
}

retCode g_le(processPo P,ptrPo a)
{
  switch(compTerm(&a[1],&a[2])){
    case smaller:
    case same:
      return Ok;
    case bigger:
      return Fail;
    default:
      return liberror(P,"=<",eINVAL);
  }
}

retCode g_gt(processPo P,ptrPo a)
{
  switch(compTerm(&a[1],&a[2])){
    case smaller:
    case same:
      return Fail;
    case bigger:
      return Ok;
    default:
      return liberror(P,">",eINVAL);
  }
}

retCode g_ge(processPo P,ptrPo a)
{
  switch(compTerm(&a[1],&a[2])){
    case bigger:
    case same:
      return Ok;
    case smaller:
      return Fail;
    default:
      return liberror(P,">=",eINVAL);
  }
}

retCode g_plus(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if(!isvar(x) && !isvar(y)){
    if(isInteger(A1)&&isInteger(A2)){
      ptrI ans = allocateInteger(&P->proc.heap,integerVal((integerPo)A1)+
				 integerVal((integerPo)A2));

      return equal(P,&ans,&a[3]);
    }
    else{
      ptrI ans = allocateFloat(&P->proc.heap,NumberVal(A1)+NumberVal(A2));

      return equal(P,&ans,&a[3]);
    }
  }
  else
    return liberror(P,"+",eINSUFARG);
}

retCode g_minus(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if(!isvar(x) && !isvar(y)){
    if(isInteger(A1)&&isInteger(A2)){
      ptrI ans = allocateInteger(&P->proc.heap,
				 integerVal((integerPo)A1)-
				 integerVal((integerPo)A2));

      return equal(P,&ans,&a[3]);
    }
    else{
      ptrI ans = allocateFloat(&P->proc.heap,NumberVal(A1)-NumberVal(A2));

      return equal(P,&ans,&a[3]);
    }
  }
  else
    return liberror(P,"-",eINSUFARG);
}

retCode g_times(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if(!isvar(x) && !isvar(y)){
    ptrI ans = allocateNumber(&P->proc.heap,NumberVal(A1)*NumberVal(A2));

    return equal(P,&ans,&a[3]);
  }
  else
    return liberror(P,"*",eINSUFARG);
}

retCode g_div(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if(isvar(x) || isvar(y))
    return liberror(P,"/",eINSUFARG);
  else{
    number divisor = NumberVal(A2);

    if(divisor==0)
      return liberror(P,"/",eDIVZERO);
    else{
      ptrI ans = allocateFloat(&P->proc.heap,NumberVal(A1)/divisor);

      return equal(P,&ans,&a[3]);
    }
  }
}

retCode g_quot(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if(isvar(x) || isvar(y))
    return liberror(P,"quot",eINSUFARG);
  else{
    number dividend = NumberVal(A1);
    number divisor = NumberVal(A2);

    if(divisor==0)
      return liberror(P,"/",eDIVZERO);
    else{
      ptrI ans = allocateInteger(&P->proc.heap,(integer)roundNumber(dividend/divisor));

      return equal(P,&ans,&a[3]);
    }
  }
}

retCode g_rem(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if(isvar(x) || isvar(y))
    return liberror(P,"rem",eINSUFARG);
  else{
    number divisor = NumberVal(A2);

    if(divisor==0)
      return liberror(P,"rem",eDIVZERO);
    else{
      ptrI ans = allocateNumber(&P->proc.heap,fmod(NumberVal(A1),divisor));

      return equal(P,&ans,&a[3]);
      }
  }
}

retCode g_abs(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"abs",eINSUFARG);
  else{
    ptrI ans = allocateNumber(&P->proc.heap,fabs(NumberVal(objV(x))));
    return equal(P,&ans,&a[2]);
  }
}

retCode g_pow(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if(!isvar(x) && !isvar(y)){
    number N = NumberVal(A1);
    number power = NumberVal(A2);
    number Rslt;

    errno = 0;			/* clear errno prior to computation */
    Rslt = pow(N,power);	/* allow for checks of the answer */

    if(errno!=0)
      return liberror(P,"pow",eINVAL);
    else{
      ptrI ans = allocateNumber(&P->proc.heap,Rslt);

      return equal(P,&a[3],&ans);
    }
  }
  else
    return liberror(P,"pow",eINSUFARG);
}

retCode g_sqrt(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number square = NumberVal(A1);

      if(square<0)
	return liberror(P,"sqrt",eINVAL);	/* square root of negative */
      else{
	number ans=sqrt(square);

	if(isvar(y)){
	  ptrI Ans = allocateNumber(&P->proc.heap,ans);

	  bindVar(P,deRef(&a[2]),Ans);
	  return Ok;
	}
	else if(IsNumber(objV(y))){
	  if(NumberVal(objV(y))==ans)
	    return Ok;
	  else
	    return Fail;
	}
	else
	  return Fail;
      }
    }
    else
      return Fail;
  }
  else
    return liberror(P,"sqrt",eINSUFARG); /* dont support inverse mode */
}

/* exponential e**x */
retCode g_exp(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number ans;

      errno = 0;		/* clear errno prior to computation */
      ans = exp(num1);		/* allow for checks of the answer */

      if(errno!=0){
	if(errno==EDOM||errno==ERANGE)
	  return liberror(P,"exp",eRANGE);
	else
	  return liberror(P,"exp",eINVAL);
      }

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"exp",eINSUFARG); /* dont support inverse mode */
}

/* logarithm loge(x) */
retCode g_log(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number ans;

      errno = 0;		/* clear errno prior to computation */
      ans = log(num1);		/* allow for checks of the answer */

      if(errno!=0){
	if(errno==EDOM||errno==ERANGE)
	  return liberror(P,"log",eRANGE);
	else
	  return liberror(P,"log",eINVAL);
      }

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"log",eINSUFARG); /* dont support inverse mode */
}

/* logarithm log10(x) */
retCode g_log10(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number ans;

      errno = 0;		/* clear errno prior to computation */
      ans = log10(num1);	/* allow for checks of the answer */

      if(errno!=0){
	if(errno==EDOM||errno==ERANGE)
	  return liberror(P,"log10",eRANGE);
	else
	  return liberror(P,"log10",eINVAL);
      }

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"log10",eINSUFARG); /* dont support inverse mode */
}

retCode g_pi(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1) && NumberVal(A1)==PI)
      return Ok;
    else
      return Fail;
  }
  else{
    ptrI Ans = allocateNumber(&P->proc.heap,PI);

    bindVar(P,deRef(&a[1]),Ans);
    return Ok;
  }
}

retCode g_trunc(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  
  if(isvar(x))
    return liberror(P,"trunc",eINSUFARG);
  else{
    objPo A1 = objV(x);
    
    if(IsNumber(A1)){
      number ans = roundNumber(NumberVal(A1));

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
}
    
retCode g_itrunc(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  
  if(isvar(x))
    return liberror(P,"trunc",eINSUFARG);
  else{
    objPo A1 = objV(x);
    
    if(isInteger(A1))
      return equal(P,&a[1],&a[2]);
    else if(isFloat(A1)){
      integer ans = (integer)roundNumber(floatVal((floatPo)A1));

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateInteger(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
}

retCode g_float(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  
  if(isvar(x))
    return liberror(P,"n2float",eINSUFARG);
  else{
    objPo A1 = objV(x);
    
    if(isInteger(A1)){
      integer A = integerVal((integerPo)A1);

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateFloat(&P->proc.heap,A);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==(number)A)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
}
    

    
retCode g_floor(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  
  if(isvar(x))
    return liberror(P,"floor",eINSUFARG);
  else{
    objPo A1 = objV(x);
    
    if(IsNumber(A1)){
      number ans = floor(NumberVal(A1));

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
}
    
retCode g_ceil(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  
  if(isvar(x))
    return liberror(P,"ceil",eINSUFARG);
  else{
    objPo A1 = objV(x);
    
    if(IsNumber(A1)){
      number ans = ceil(NumberVal(A1));

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
}
    

/* Trigonometric functions */
retCode g_sin(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number ans = sin(num1);

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"sin",eINSUFARG); /* dont support inverse mode */
}

retCode g_cos(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number ans = cos(num1);

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"cos",eINSUFARG); /* dont support inverse mode */
}

retCode g_tan(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number ans = tan(num1);

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"tan",eINSUFARG); /* dont support inverse mode */
}

retCode g_asin(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number ans;

      errno = 0;		/* clear errno prior to computation */
      ans = asin(num1);		/* allow for checks of the answer */

      if(errno!=0){
	if(errno==EDOM)
	  return liberror(P,"asin",eRANGE);
	else
	  return liberror(P,"asin",eINVAL);
      }

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"asin",eINSUFARG); /* dont support inverse mode */
}

retCode g_acos(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number ans;

      errno = 0;		/* clear errno prior to computation */
      ans = acos(num1);		/* allow for checks of the answer */

      if(errno!=0){
	if(errno==EDOM)
	  return liberror(P,"acos",eRANGE);
	else
	  return liberror(P,"acos",eINVAL);
      }

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"acos",eINSUFARG); /* dont support inverse mode */
}

retCode g_atan(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number ans = atan(num1);

      if(isvar(y)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[2]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"atan",eINSUFARG); /* dont support inverse mode */
}

/*
 * srand(n) - reinitialize the random numbers generator
 */

retCode g_srand(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      long i = NumberVal(A1);

      if(i<0)
	return liberror(P,"srand",eRANGE);
      else{
	srand(i);
	return Ok;
      }
    }
    else
      return Fail;
  }
  else
    return liberror(P,"srand",eINSUFARG);
}

/* rand(X) => Random No. in range [0..X) */

retCode g_rand(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);

      if(num1>0){
	number ans = num1*((number)rand())/(RAND_MAX+1.0);

	if(isvar(y)){		/* check the output argument */
	  ptrI Ans = allocateNumber(&P->proc.heap,ans);

	  bindVar(P,deRef(&a[2]),Ans);
	  return Ok;
	}
	else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	  return Ok;
	else
	  return Fail;
      }
      else
	return liberror(P,"rand",eINVAL);	/* invalid argument */
    }
    else
      return Fail;
  }
  else
    return liberror(P,"rand",eINSUFARG); /* inverse mode doesnt make sense */
}

/* irand(X) => Random integer in range [0..X) */

retCode g_irand(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);

      if(roundNumber(num1)==num1 && num1>0){
	integer ans = (integer)(num1*((number)rand())/(RAND_MAX+1.0));

	if(isvar(y)){		/* check the output argument */
	  ptrI Ans = allocateInteger(&P->proc.heap,ans);

	  bindVar(P,deRef(&a[2]),Ans);
	  return Ok;
	}
	else if(IsNumber(objV(y)) && NumberVal(objV(y))==ans)
	  return Ok;
	else
	  return Fail;
      }
      else
	return liberror(P,"irand",eINVAL); /* invalid argument */
    }
    else
      return Fail;
  }
  else
    return liberror(P,"irand",eINSUFARG); /* inverse mode doesnt make sense */
}

/*
 * Functions to assist in the manipulations of the fp number 
 */

/*  multiply A[1] by A[2]=integral power of 2, used in generating fp */

retCode g_ldexp(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if(!isvar(x) && !isvar(y)){
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if(IsNumber(A1) && IsNumber(A2)){
      number num1 = NumberVal(A1);
      number ans = ldexp(num1,(long)NumberVal(A2));

      if(isvar(z)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,ans);

	bindVar(P,deRef(&a[3]),Ans);
	return Ok;
      }
      else if(IsNumber(objV(y)) && NumberVal(objV(z))==ans)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"ldexp",eINSUFARG); /* dont support inverse mode */
}

/* Convert fp to fractional and integral exponent parts */
retCode g_frexp(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(isFloat(A1)){
      number num1 = NumberVal(A1);
      int exp;
      number frac = frexp(num1,&exp);

      if(isvar(y)){		/* check the output arguments */
	ptrI Ans = allocateNumber(&P->proc.heap,frac);

	bindVar(P,deRef(&a[2]),Ans);
      }
      else if(isFloat(objV(y)) && NumberVal(objV(y))!=frac)
	return Fail;

      if(isvar(z)){		/* check the output argument */
	ptrI Ans = allocateNumber(&P->proc.heap,exp);

	bindVar(P,deRef(&a[3]),Ans);
	return Ok;
      }
      else if(isFloat(objV(z)) && NumberVal(objV(z))==exp)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"frexp",eINSUFARG); /* dont support inverse mode */
}

/* Convert fp to fractional and integral parts */
retCode g_modf(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if(!isvar(x)){
    objPo A1 = objV(x);

    if(IsNumber(A1)){
      number num1 = NumberVal(A1);
      number intgrl;
      number frac = modf(num1,&intgrl);

      if(isvar(y)){		/* check the output arguments */
	ptrI Ans = allocateFloat(&P->proc.heap,intgrl);

	bindVar(P,deRef(&a[2]),Ans);
      }
      else if(isFloat(objV(y)) && NumberVal(objV(z))!=intgrl)
	return Fail;

      if(isvar(z)){		/* check the output argument */
	ptrI Ans = allocateFloat(&P->proc.heap,frac);

	bindVar(P,deRef(&a[3]),Ans);
	return Ok;
      }
      else if(isFloat(objV(z=deRefI(&a[3]))) && NumberVal(objV(z))==frac)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return liberror(P,"modf",eINSUFARG); /* dont support inverse mode */
}

