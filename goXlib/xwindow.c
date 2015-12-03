/* 
  Window management
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

// A top-level window has a name and other attributes
retCode x_topLevelWindow(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);		/* The x coordinate of origin */
  ptrI y = deRefI(&a[2]);		/* The y coordinate of origin */
  ptrI w = deRefI(&a[3]);		/* The width of the window */
  ptrI h = deRefI(&a[4]);		/* The height of the window */
  ptrI bc = deRefI(&a[5]);		/* The background color pixel */
  ptrI n = deRefI(&a[6]);		/* The window name */
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(x)||isvar(y)||isvar(w)||isvar(h)||isvar(bc)||isvar(n))
    return liberror(P,"xTopLevelWindow",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xTopLevelWindow",eNOX);
  else{
    uniChar winName[MAX_MSG_LEN];
    char win_name[MAX_MSG_LEN];
    char *w_name = (char*)win_name;
    
    if(isGroundString(&n)!=Ok)
      return liberror(P,"xTopLevelWindow",eSTRNEEDD);

    String2Uni(&n,winName,NumberOf(winName));
    _utf(winName,(unsigned char*)&win_name[0],NumberOf(win_name));

    int width = (int)integerVal(intV(w));
    int height = (int)integerVal(intV(h));

    Window win = XCreateSimpleWindow(display,RootWindow(display,screen_num),
				     (int)integerVal(intV(x)),
				     (int)integerVal(intV(y)),
				     width,
				     height,
				     0,0,
				     (unsigned int)integerVal(intV(bc)));

    XTextProperty windowName;

    if(XStringListToTextProperty((char**)&w_name,1,&windowName)==0)
      return liberror(P,"xTopLevelWindow",eNOPERM);

    XSizeHints *size_hints = XAllocSizeHints();
    if(size_hints==NULL)
      return liberror(P,"xTopLevelWindow",eSPACE);

    size_hints->flags = PPosition|PSize|PMinSize;
    size_hints->min_width = 300;
    size_hints->min_height = 200;

    XWMHints *wm_hints = XAllocWMHints();
    if(wm_hints==NULL)
      return liberror(P,"xTopLevelWindow",eSPACE);

    wm_hints->initial_state = NormalState;
    wm_hints->input = True;
    //   wm_hints->icon_pixmap = icon_pixmap;
    wm_hints->flags = StateHint | /*IconPixmapHint | */InputHint;

    XClassHint *class_hints = XAllocClassHint();
    if(class_hints==NULL)
      return liberror(P,"xTopLevelWindow",eSPACE);

    class_hints->res_name = "GoProgram"; /* This will need to be fixed */
    class_hints->res_class = "Go";

    XSetWMProperties(display, win, &windowName, NULL,
		     NULL, 0, size_hints, wm_hints, class_hints);
    /* Select event types wanted -- initially only a minimal set */
    XSelectInput(display, win, ExposureMask | StructureNotifyMask);

    ptrI W = allocateInteger(&P->proc.heap,(int)win);
    return equal(P,&W,&a[7]);
  }
}

retCode x_createWindow(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);		/* The x coordinate of origin */
  ptrI y = deRefI(&a[2]);		/* The y coordinate of origin */
  ptrI w = deRefI(&a[3]);		/* The width of the window */
  ptrI h = deRefI(&a[4]);		/* The height of the window */
  ptrI p = deRefI(&a[5]);		/* The parent window */
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(x)||isvar(y)||isvar(w)||isvar(h)||isvar(p))
    return liberror(P,"xCreateWindow",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xCreateWindow",eNOX);
  else{
    Window win = XCreateSimpleWindow(display,(Window)integerVal(intV(p)),
				     (int)integerVal(intV(x)),
				     (int)integerVal(intV(y)),
				     (int)integerVal(intV(w)),
				     (int)integerVal(intV(h)),
				     0,0,
				     WhitePixel(display,screen_num));

    ptrI W = allocateInteger(&P->proc.heap,(int)win);

    XSelectInput(display, win, ExposureMask | 
		 StructureNotifyMask | EnterWindowMask | LeaveWindowMask);

    return equal(P,&W,&a[6]);
  }
}

retCode x_mapWindow(processPo P,ptrPo a)
{
  ptrI w = deRefI(&a[1]);		/* The window ID to map */
  ptrI r = deRefI(&a[2]);		/* Do we want it on top? */
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(w)||isvar(r))
    return liberror(P,"xMapWindow",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xMapWindow",eNOX);
  else{
    Window win = (Window)integerVal(intV(w));
    int ret = 0;

    if(w==trueClass)
      ret = XMapRaised(display,win);
    else
      ret = XMapWindow(display,win);

    XFlush(display);

    if(ret==BadWindow)
      return liberror(P,"xMapWindow",eINVAL);

    return Ok;
  }
}

retCode x_unmapWindow(processPo P,ptrPo a)
{
  ptrI w = deRefI(&a[1]);		/* The window ID to map */
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(w))
    return liberror(P,"xUnmapWindow",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xUnmapWindow",eNOX);
  else{
    Window win = (Window)integerVal(intV(w));
    int ret = XUnmapWindow(display,win);

    if(ret==BadWindow)
      return liberror(P,"xUnmapWindow",eINVAL);

    XFlush(display);

    return Ok;
  }
}

retCode x_windowAttributes(processPo P,ptrPo a)
{
  ptrI w = deRefI(&a[1]);		/* The window ID to get attributes of */
  Display *display = getDisplay();	/* Find out if we are connected */

  return Ok;
}
