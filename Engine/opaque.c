/* 
  Opaque type interface for Go!
  (c) 2000 F.G.McCabe

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

  $Id: opaque.c,v 1.4 2004/04/29 16:24:27 fmccabe Exp $
  $Log: opaque.c,v $
  Revision 1.4  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/
#include "config.h"		/* pick up standard configuration header */
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include "go.h"
#include "symbols.h"
#include "hash.h"
#include "opaque.h"

ptrI permOpaque(heapPo H,opaqueTp type,size_t len,void *data)
{
  opaquePo new=(opaquePo)permAllocate(H,OpaqueCellCount(len));

  new->sign = OpaqueMark(len);
  new->type = type;
  memcpy(new->data,data,len);

  return objP(new);
}

typedef struct {
  opaqueHdlr h;
  void *cl;
} opaqueHRec;

static opaqueHRec handlers[lastHandle];

void initOpaque(void)
{
  static logical inited = False;
  
  if(!inited){
    unsigned int i;

    for(i=0;i<NumberOf(handlers);i++)
      handlers[i].h = NULL;
    inited = True;
  }
}

void registerOpaqueType(opaqueHdlr h,opaqueTp type,void *cl)
{
  initOpaque();

  assert(type>=0 && type<lastHandle);

  assert(handlers[type].h==NULL);

  handlers[type].h = h;
  handlers[type].cl = cl;
}

retCode displayOpaque(ioPo f,opaquePo p)
{
  opaqueTp type = OpaqueType(p);

  assert(type>=0 && type<lastHandle);

  if(handlers[type].h!=NULL)
    return handlers[type].h(showOpaque,OpaqueVal(p),f,handlers[type].cl);
  else
    return outMsg(f,"Unhandled %d opaque",type);
}

void mrkOpaque(gcSupportPo G,opaquePo p)
{
  opaqueTp type = OpaqueType(p);

  assert(type>=0 && type<lastHandle);

  if(handlers[type].h!=NULL)
    handlers[type].h(markOpaque,OpaqueVal(p),G,handlers[type].cl);
}

void adjstOpaque(gcSupportPo G,opaquePo p)
{
  opaqueTp type = OpaqueType(p);

  assert(type>=0 && type<lastHandle);

  if(handlers[type].h!=NULL)
    handlers[type].h(adjustOpaque,OpaqueVal(p),G,handlers[type].cl);
}

void clsOpaque(opaquePo p)
{
  opaqueTp type = OpaqueType(p);

  assert(type>=0 && type<lastHandle);

  if(handlers[type].h!=NULL)
    handlers[type].h(closeOpaque,OpaqueVal(p),NULL,handlers[type].cl);
}



