/* 
   Color management for the Xlib library
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

retCode x_rgbColor(processPo P,ptrPo a)
{
  ptrI r = deRefI(&a[1]);		/* The red component */
  ptrI g = deRefI(&a[2]);		/* The green component */
  ptrI b = deRefI(&a[3]);		/* The blue component */
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(r) || isvar(b) || isvar(g))
    return liberror(P,"xRGBColor",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xRGBColor",eNOX);
  else{
    Colormap default_cmap   = DefaultColormap(display, screen_num);
    XColor color = { 0,
		     (unsigned short)IntVal(r),
		     (unsigned short)IntVal(g),
		     (unsigned short)IntVal(b)};

    if(XAllocColor(display,default_cmap,&color)){
      ptrI Px = allocateInteger(&P->proc.heap,color.pixel);
      return equal(P,&Px,&a[4]);
    }
    else
      return liberror(P,"xRGBColor",eINVAL);
  }
}

// Compute a named color
retCode x_Color(processPo P,ptrPo a)
{
  uniChar colorName[MAX_MSG_LEN];
  char color_name[MAX_MSG_LEN];
  ptrI c = deRefI(&a[1]);
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(c)||isGroundString(&c)!=Ok)
    return liberror(P,"xColor",eSTRNEEDD);
  else if(display==NULL)
    return liberror(P,"xColor",eNOX);
  else{
    Colormap cmap = DefaultColormap(display, screen_num);
    XColor color;

    String2Uni(&c,colorName,NumberOf(colorName));
    _utf(colorName,(unsigned char*)color_name,NumberOf(color_name));

    if(!XParseColor(display,cmap,color_name,&color))
      return liberror(P,"xColor",eINVAL);
    else if(XAllocColor(display,cmap,&color)){
      ptrI Px = allocateInteger(&P->proc.heap,color.pixel);
      retCode ret = equal(P,&Px,&a[5]);
      if(ret==Ok){
	ptrI R = allocateInteger(&P->proc.heap,color.red);
	ret = equal(P,&R,&a[2]);
      }
      if(ret==Ok){
	ptrI G = allocateInteger(&P->proc.heap,color.green);
	ret = equal(P,&G,&a[3]);
      }
      if(ret==Ok){
	ptrI B = allocateInteger(&P->proc.heap,color.blue);
	ret = equal(P,&B,&a[4]);
      }
      return ret;
    }
    else
      return liberror(P,"xColor",eINVAL);
  }
}
