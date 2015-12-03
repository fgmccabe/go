/* 
  Text drawing primitives
  (c) 2006 F.G.McCabe

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
#include "config.h"
#include "go.h"		/* Main header file */
#include "lock.h"
#include "debug.h"
#include "opaque.h"
#include "goX.h"

retCode x_drawString(processPo P,ptrPo a)
{
  ptrI win = deRefI(&a[1]);		/* The window to draw into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI x = deRefI(&a[3]);		/* X-Coord of top-left */
  ptrI y = deRefI(&a[4]);		/* Y-Coord of top-left */
  ptrI t = deRefI(&a[5]);		/* The string to display */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(win)||isvar(g)||isvar(x)||isvar(y)||isvar(t))
    return liberror(P,"xDrawString",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xDrawString",eNOX);
  else{
    uniChar Buff[MAX_MSG_LEN];
    uniChar *Text = Buff;
    char buff[MAX_MSG_LEN];
    char *text = (char*)buff;
    
    if(isGroundString(&t)!=Ok)
      return liberror(P,"xDrawString",eSTRNEEDD);

    long Len = StringLen(&t)+1;

    if(Len>NumberOf(Buff))
      Text = (uniChar*)malloc(sizeof(uniChar)*Len);

    String2Uni(&t,Text,Len);

    long len = Len*4;			/* Maximum expansion of unicode */
    if(len>NumberOf(buff))
      text = (char*)malloc(sizeof(char)*len);
  
    long sLen = uni_utf8(Text,uniStrLen(Text),
			 (unsigned char*)text,len); /* Convert to UTF8 */

    int X = (int)NumberVal(objV(x));
    int Y = (int)NumberVal(objV(y));
    Drawable D = (Drawable)integerVal(intV(win));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    XDrawString(display,D,gc,X,Y,text,sLen);

    if(text!=buff)
      free(text);
    if(Text!=Buff)
      free(Text);
    return Ok;
  }
}

retCode x_stringExtent(processPo P,ptrPo a)
{
  ptrI g = deRefI(&a[1]);		/* The graphics context */
  ptrI t = deRefI(&a[2]);		/* The string to compute bounds of */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(g)||isvar(t))
    return liberror(P,"xStringExtent",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xStringExtent",eNOX);
  else{
    char buff[MAX_MSG_LEN];
    char *text = (char*)buff;
    
    if(isGroundString(&t)!=Ok)
      return liberror(P,"xStringExtent",eSTRNEEDD);

    long Len = StringLen(&t)*3+1;	/* A fuzz factor in the conversion */

    if(Len>NumberOf(buff))
      text = (char*)malloc(sizeof(char)*Len);

    String2Utf8(&t,text,Len);		/* Convert to UTF8 */

    Font fnt = integerVal(intV(g));
    XFontStruct *fS = XQueryFont(display,fnt);
    int direction,ascent,descent;
    XCharStruct overall;

    XTextExtents(fS,text,strlen(text),&direction,&ascent,&descent,&overall);

    retCode ret = Ok;

    ptrI tmp = allocateFloat(&P->proc.heap,overall.ascent);
    ret = equal(P,&tmp,&a[3]);		/* return ascent */

    if(ret==Ok){
      tmp = allocateFloat(&P->proc.heap,overall.descent);
      ret = equal(P,&tmp,&a[4]);
    }

    if(ret==Ok){
      tmp = allocateFloat(&P->proc.heap,overall.width);
      ret = equal(P,&tmp,&a[5]);
    }

    if(text!=buff)
      free(text);

    return ret;
  }
}

/*
 * An opaque structure to hold a Font structure
 */

retCode x_loadFont(processPo P,ptrPo a)
{
  ptrI f = deRefI(&a[1]);		/* The font name */
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(f))
    return liberror(P,"xLoadFont",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xLoadFont",eNOX);
  else{
    char buff[MAX_MSG_LEN];
    String2Utf8(&f,buff,NumberOf(buff));
    Font font = XLoadFont(display,buff);
    XFontStruct *fS = XQueryFont(display,font);

    ptrI fI = allocateInteger(&P->proc.heap,(integer)font);
    return equal(P,&fI,&a[2]);
  }
}

retCode x_queryFont(processPo P,ptrPo a)
{
  ptrI f = deRefI(&a[1]);		/* The font ID */
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(f))
    return liberror(P,"xQueryFont",eINSUFARG);
  else if(!IsInt(f))
    return liberror(P,"xQueryFont",eINVAL);
  else if(display==NULL)
    return liberror(P,"xQueryFont",eNOX);
  else{
    XFontStruct *s = XQueryFont(display,IntVal(f));

    if(s!=NULL){
      ptrI res = allocateFloat(&P->proc.heap,s->ascent);
      retCode ret = equal(P,&res,&a[2]);

      if(ret==Ok){
	res = allocateFloat(&P->proc.heap,s->descent);
	ret = equal(P,&res,&a[3]);
      }
      return ret;
    }
    else
      return liberror(P,"xQueryFont",eINVAL);
  }
}

retCode x_charInfo(processPo P,ptrPo a)
{
  ptrI f = deRefI(&a[1]);		/* The font ID */
  ptrI c = deRefI(&a[2]);		/* The character */
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(f)||isvar(c))
    return liberror(P,"xCharInfo",eINSUFARG);
  else if(!IsInt(f)||!IsChar(c))
    return liberror(P,"xCharInfo",eINVAL);
  else if(display==NULL)
    return liberror(P,"xCharInfo",eNOX);
  else{
    XFontStruct *s = XQueryFont(display,(XID)IntVal(f));
    uniChar ch = CharVal(charV(c));

    if(s!=NULL){
      if(ch>=s->min_char_or_byte2 &&
	 ch<s->max_char_or_byte2){
	XCharStruct *chS = &s->per_char[ch-s->min_char_or_byte2];
	ptrI res = allocateFloat(&P->proc.heap,chS->lbearing);
	retCode ret = equal(P,&res,&a[3]);

	if(ret==Ok){
	  res = allocateFloat(&P->proc.heap,chS->rbearing);
	  ret = equal(P,&res,&a[4]);
	}

	if(ret==Ok){
	  res = allocateFloat(&P->proc.heap,chS->width);
	  ret = equal(P,&res,&a[5]);
	}

	if(ret==Ok){
	  res = allocateFloat(&P->proc.heap,chS->ascent);
	  ret = equal(P,&res,&a[6]);
	}

	if(ret==Ok){
	  res = allocateFloat(&P->proc.heap,chS->descent);
	  ret = equal(P,&res,&a[7]);
	}
	
	return ret;
      }
      else
	return liberror(P,"xCharInfo",eNOTFND); /* Not defined in this font */
    }
    else
      return liberror(P,"xCharInfo",eINVAL);
  }
}

    
static retCode fontOpaqueHdlr(opaqueEvalCode code,void **p,void *cd,void *cl);

void xInitFonts(void)
{
  registerOpaqueType(fontOpaqueHdlr,fontHandle,NULL);
}

static retCode fontOpaqueHdlr(opaqueEvalCode code,void **p,void *cd,void *cl)
{
  XFontStruct *ff = *(XFontStruct**)p;

  if(ff!=NULL){
    switch(code){
    case showOpaque:{
      ioPo f = (ioPo)cd;
      outMsg(f,"<<font %x>>",ff);
      return Ok;
    }
    case markOpaque:
    case adjustOpaque:
      return Ok;
      
    case closeOpaque:{                    /* We simply free the font structure */
      *(XFontStruct**)p = NULL;
      XFreeFont(getDisplay(),ff);
      return Ok; 
    }
      
    default:
    return Error;
    }
  }
  else
    return Ok;
}

ptrI allocOpaqueFontPtr(heapPo H,XFontStruct *font)
{
  return permOpaque(H,fontHandle,sizeof(XFontStruct*),(void *)font);
}

XFontStruct *opaqueFontPtr(ptrI p)
{
  return *(XFontStruct**)OpaqueVal(opaqueV(p));
}

void clearOpaqueFontPointer(ptrI p)
{
  *(XFontStruct**)OpaqueVal(opaqueV(p)) = NULL;
}

