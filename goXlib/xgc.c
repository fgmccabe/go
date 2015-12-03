/* 
  Xlib graphics context management
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
#include "goX.h"

#ifdef XTRACE
#endif

static unsigned int getGCValues(ptrI l,Display *display,XGCValues *v)
{
  unsigned int mask = 0;		/* The mask of attributes that we set */

  while(IsList(l)){
    ptrI e = deRefI(listHead(objP(l)));

    if(!isvar(e)){
      objPo o = objV(e);

      if(e==kGXclear){
	mask |= GCFunction;
	v->function = GXclear;
      }
      else if(e==kGXand){
	mask |= GCFunction;
	v->function = GXand;
      }
      else if(e==kGXandReverse){
	mask |= GCFunction;
	v->function = GXandReverse;
      }
      else if(e==kGXcopy){
	mask |= GCFunction;
	v->function = GXcopy;
      }
      else if(e==kGXnoop){
	mask |= GCFunction;
	v->function = GXnoop;
      }
      else if(e==kGXxor){
	mask |= GCFunction;
	v->function = GXxor;
      }
      else if(e==kGXnor){
	mask |= GCFunction;
	v->function = GXnor;
      }
      else if(e==kGXequiv){
	mask |= GCFunction;
	v->function = GXequiv;
      }
      else if(e==kGXinvert){
	mask |= GCFunction;
	v->function = GXinvert;
      }
      else if(e==kGXorReverse){
	mask |= GCFunction;
	v->function = GXorReverse;
      }
      else if(e==kGXcopyInverted){
	mask |= GCFunction;
	v->function = GXcopyInverted;
      }
      else if(e==kGXorInverted){
	mask |= GCFunction;
	v->function = GXorInverted;
      }
      else if(e==kGXnand){
	mask |= GCFunction;
	v->function = GXnand;
      }
      else if(e==kGXset){
	mask |= GCFunction;
	v->function = GXset;
      }
      else if(IsStructure(o,kxForeground,1)){
	ptrI f = deRefI(ConsEl((consPo)o,0));

	if(!isvar(f)){
	  mask |= GCForeground;
	  v->foreground = ((unsigned long)IntegerVal(intV(f)))&0xffffff;
	}
      }
      else if(IsStructure(o,kxBackground,1)){
	ptrI f = deRefI(ConsEl((consPo)o,0));

	if(!isvar(f)){
	  mask |= GCBackground;
	  v->background = ((unsigned long)IntegerVal(intV(f)))&0xffffff;
	}
      }
      else if(IsStructure(o,kxLineWidth,1)){
	ptrI f = deRefI(ConsEl((consPo)o,0));

	if(!isvar(f)){
	  mask |= GCLineWidth;
	  v->line_width = (int)IntegerVal(intV(f));
	}
      }
      else if(e==kLineSolid){
	mask |= GCLineStyle;
	v->line_style = LineSolid;
      }
      else if(IsStructure(o,kxLineOnOffDash,2)){
	ptrI d = deRefI(ConsEl((consPo)o,0));
	ptrI f = deRefI(ConsEl((consPo)o,1));

	if(!isvar(d) && !isvar(f)){
	  mask |= GCLineStyle | GCDashOffset | GCDashList;
	  v->line_style = LineOnOffDash;
	  v->dash_offset = (int)IntegerVal(intV(f));
	  v->dashes = (char)IntegerVal(intV(d));
	}
      }
      else if(IsStructure(o,kxLineDoubleDash,2)){
	ptrI d = deRefI(ConsEl((consPo)o,0));
	ptrI f = deRefI(ConsEl((consPo)o,1));

	if(!isvar(d) && !isvar(f)){
	  mask |= GCLineStyle | GCDashOffset | GCDashList;
	  v->line_style = LineDoubleDash;
	  v->dash_offset = (int)IntegerVal(intV(f));
	  v->dashes = (char)IntegerVal(intV(d));
	}
      }
      else if(e==kCapButt){
	mask |= GCCapStyle;
	v->cap_style = CapButt;
      }
      else if(e==kCapNoLast){
	mask |= GCCapStyle;
	v->cap_style = CapNotLast;
      }
      else if(e==kCapRound){
	mask |= GCCapStyle;
	v->cap_style = CapRound;
      }
      else if(e==kCapProjecting){
	mask |= GCCapStyle;
	v->cap_style = CapProjecting;
      }
      else if(e==kJoinMiter){
	mask |= GCJoinStyle;
	v->join_style = JoinMiter;
      }
      else if(e==kJoinRound){
	mask |= GCJoinStyle;
	v->join_style = JoinRound;
      }
      else if(e==kJoinBevel){
	mask |= GCJoinStyle;
	v->join_style = JoinBevel;
      }
      else if(IsStructure(o,kxFontName,1)){
	ptrI d = deRefI(ConsEl((consPo)o,0));

	if(!isvar(d)){
	  char fn[MAX_MSG_LEN];
	  String2Utf8(&d,fn,NumberOf(fn));
	  Font font = XLoadFont(display,fn);
	  if(font!=NULL){
	    mask |= GCFont;
	    v->font = font;
	  }
	}
      }


      // Fix with more later
    }

    l = deRefI(listTail(listV(l)));
  }
  return mask;
}

retCode x_createGC(processPo P,ptrPo a)
{
  Display *display = getDisplay();	/* Find out if we are connected */
  ptrI o = deRefI(&a[1]);		/* The list of graphics options */
  
  if(isvar(o))
    return liberror(P,"xcreateGC",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xCreateGC",eNOX);
  else{
    XGCValues gcV;
    unsigned int mask = getGCValues(o,display,&gcV);
    GC gc = XCreateGC(display,RootWindow(display,screen_num),mask,&gcV);
    ptrI GC = allocateInteger(&P->proc.heap,(unsigned long)gc);
    return equal(P,&GC,&a[2]);
  }
}

retCode x_destroyGC(processPo P,ptrPo a)
{
  Display *display = getDisplay();	/* Find out if we are connected */
  ptrI g = deRefI(&a[1]);		/* The graphics context */
  
  if(isvar(g))
    return liberror(P,"xDestroyGC",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xDestroyGC",eNOX);
  else{
    GC gc = (GC)((unsigned long)IntegerVal(intV(g)));
    XFreeGC(display,gc);
    return Ok;
  }
}

retCode x_setGC(processPo P,ptrPo a)
{
  Display *display = getDisplay();	/* Find out if we are connected */
  ptrI g = deRefI(&a[1]);		/* The graphics context */
  ptrI o = deRefI(&a[2]);		/* The list of graphics options */
  
  if(isvar(g)||isvar(o))
    return liberror(P,"xSetGC",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xSetGC",eNOX);
  else{
    XGCValues gcV;
    unsigned int mask = getGCValues(o,display,&gcV);
    GC gc = (GC)((unsigned long)IntegerVal(intV(g)));

    XChangeGC(display,gc,mask,&gcV);
    return Ok;
  }
}
