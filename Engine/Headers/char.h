/*
 * Header file for the character management of Go! engine
   (c) 2001-2007 F.G.McCabe

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

#ifndef _GO_CHAR_H_
#define _GO_CHAR_H_

#include "word.h"

/* Character structure */
typedef struct _char_record_ {
  ptrI sign;				/* == charMark */
  uniChar uni;				/* The character itself */
} charRec, *charPo;

extern ptrI charClass;			/* charClass is a special class */

static inline logical isChr(objPo p)
{
  return hasClass(p,charClass);
}

static inline logical IsChar(ptrI x)
{
  return HasClass(x,charClass);
}

static inline charPo charV(ptrI x)
{
  assert(isobj(x) && IsChar(x));
  return (charPo)objV(x);
}

static inline uniChar CharVal(charPo p)
{
  assert(isChr((objPo)p));

  return p->uni;
}

void initCharClass(void);
void restartChars(globalGcPo G);
ptrI newChar(const uniChar ch);
retCode wStringChr(ioPo f,uniChar ch);

retCode g_isCcChar(processPo P,ptrPo a);         /* Other, Control */
retCode g_isCfChar(processPo P,ptrPo a);         /* Other, Format */
retCode g_isCnChar(processPo P,ptrPo a);         /* Other, Unassigned */
retCode g_isCoChar(processPo P,ptrPo a);         /* Other, Private */
retCode g_isCsChar(processPo P,ptrPo a);         /* Other, surrogate */
retCode g_isLlChar(processPo P,ptrPo a);         /* Letter, lowercase */
retCode g_isLmChar(processPo P,ptrPo a);         /* Letter, modifier */
retCode g_isLoChar(processPo P,ptrPo a);         /* Letter, other */
retCode g_isLtChar(processPo P,ptrPo a);         /* Letter, titlecase */
retCode g_isLuChar(processPo P,ptrPo a);         /* Letter, uppercase */
retCode g_isMcChar(processPo P,ptrPo a);         /* Mark, spacing combining */
retCode g_isMeChar(processPo P,ptrPo a);         /* Mark, enclosing */
retCode g_isMnChar(processPo P,ptrPo a);         /* Mark, non spacing */
retCode g_isNdChar(processPo P,ptrPo a);         /* Number, decimal digit */
retCode g_isNlChar(processPo P,ptrPo a);         /* Number, letter */
retCode g_isNoChar(processPo P,ptrPo a);         /* Number, other */
retCode g_isPcChar(processPo P,ptrPo a);         /* Punctuation, connector */
retCode g_isPdChar(processPo P,ptrPo a);         /* Punctuation, dash */
retCode g_isPeChar(processPo P,ptrPo a);         /* Punctuation, close */
retCode g_isPfChar(processPo P,ptrPo a);         /* Punctuation, final quote */
retCode g_isPiChar(processPo P,ptrPo a);         /* Punctuation, initial quote */
retCode g_isPoChar(processPo P,ptrPo a);         /* Punctuation, other */
retCode g_isPsChar(processPo P,ptrPo a);         /* Punctution, open */
retCode g_isScChar(processPo P,ptrPo a);         /* Symbol, currency */
retCode g_isSkChar(processPo P,ptrPo a);         /* Symbol, modifier */
retCode g_isSmChar(processPo P,ptrPo a);         /* Symbol, math */
retCode g_isSoChar(processPo P,ptrPo a);         /* Symbol, other */
retCode g_isZlChar(processPo P,ptrPo a);         /* Separator, line */
retCode g_isZpChar(processPo P,ptrPo a);         /* Separator, paragraph */
retCode g_isZsChar(processPo P,ptrPo a);         /* Separator, space */
retCode g_isLetterChar(processPo P,ptrPo a);
retCode g_digitCode(processPo P,ptrPo a);
retCode g_charOf(processPo P,ptrPo a);

#endif
