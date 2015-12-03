/* 
  Xlib event management
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
static char *event_name[] = {
   "",
   "",
   "KeyPress",
   "KeyRelease",
   "ButtonPress",
   "ButtonRelease",
   "MotionNotify",
   "EnterNotify",
   "LeaveNotify",
   "FocusIn",
   "FocusOut",
   "KeymapNotify",
   "Expose",
   "GraphicsExpose",
   "NoExpose",
   "VisibilityNotify",
   "CreateNotify",
   "DestroyNotify",
   "UnmapNotify",
   "MapNotify",
   "MapRequest",
   "ReparentNotify",
   "ConfigureNotify",
   "ConfigureRequest",
   "GravityNotify",
   "ResizeRequest",
   "CirculateNotify",
   "CirculateRequest",
   "PropertyNotify",
   "SelectionClear",
   "SelectionRequest",
   "SelectionNotify",
   "ColormapNotify",
   "ClientMessage",
   "MappingNotify"
};
#endif

/*
 * xNextEvent computes the next event
 * Low-level interface that has to be mapped by a Go! wrapper
 * xNextEvent(Win-,Type-,...)
 * where Win and Type are present for all events
 * Win is a window ID
 * and Type is a symbol that indicates the type of the event.
 * The remaining arguments depend on the type of event
 * KeyPress: Type, Win, Time, x, y, KeyCode, KeyMask, .., .., .., .., Stroke
 * KeyRelease: Type, Win, Time, x, y, KeyCode, KeyMask, .., .., .., .., Stroke
 * ButtonPress: Type, Win, Time, x, y, xRoot, yRoot, Mask, Btn, ..
 * ButtonRelease: Type, Win, Time, x, y, xRoot, yRoot, Mask, Btn, ..
 * motionNotify: Type, Win, Time, x, y, xRoot, yRoot, Mask, ..
 * enterNotify: Type, Win, Time, x, y, xRoot, yRoot, Mask, Focus, ..
 * leaveNotify: Type, Win, Time, x, y, xRoot, yRoot, Mask, Focus, ..
 * focusInNotify: Type, Win,..
 * focusOutNotify: Type, Win, ..
 * exposeEvent: Type, Win, x, y, width, height, count, ..
 * createNotify: Type, Win, x, y, width, height, border_width, ..
 * destroyNotify: Type, Win, ..
 * mapNotify: Type, Win, ..
 * unmapNotify: Type, Win, ..
 * reparentNotify: Type, Win, Parent, x, y, ..
 * configureNotify: Type, Win, x, y, width, height, border_width, ..
 */

retCode x_nextEvent(processPo P,ptrPo a)
{
  XEvent event;
  Display *display = getDisplay();	/* Find out if we are connected */

  if(display==NULL)
    return liberror(P,"xNextEvent",eNOX);

 again:				      /* Some events are processed internally */

#ifdef XTRACE
  if(traceX)
    outMsg(logFile, "lookng for next X event\n");
#endif

  XNextEvent(display,&event);

#ifdef XTRACE
  if(traceX)
    outMsg(logFile, "got a %s event\n", event_name[event.type]);
#endif

  ptrI eventCode = allocateInteger(&P->proc.heap,event.type);
  rootPo root = gcAddRoot(&P->proc.heap,&eventCode);
  

  switch(event.type){
  case KeyPress:{
    XKeyEvent *keyEvent = (XKeyEvent*)&event;
    // Key as a character string
    char stroke[16];
    KeySym keySym;
    int kLen = XLookupString(keyEvent,
			     (char*)&stroke[0],NumberOf(stroke),
			     &keySym,NULL);
    stroke[kLen]='\0';

    ptrI tmp = allocateInteger(&P->proc.heap,(integer)keyEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)keyEvent->time);
      ret = equal(P,&tmp,&a[3]);
    }

    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)keyEvent->x);
      ret = equal(P,&tmp,&a[4]);
    }

    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)keyEvent->y);
      ret = equal(P,&tmp,&a[5]);
    }

    // Keycode number
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)keySym);
      ret = equal(P,&tmp,&a[6]);
    }

    // Key mask
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)(keyEvent->state));
      ret = equal(P,&tmp,&a[7]);
    }

    // Key stroke string
    if(ret==Ok){
      tmp = allocateCString(&P->proc.heap,stroke);
      ret = equal(P,&tmp,&a[10]);
    }
    
    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case KeyRelease:{
    XKeyEvent *keyEvent = (XKeyEvent*)&event;
    // Key as a character string
    char stroke[16];
    KeySym keySym;
    int kLen = XLookupString(keyEvent,
			     (char*)&stroke[0],NumberOf(stroke),
			     &keySym,NULL);
    stroke[kLen]='\0';

    ptrI tmp = allocateInteger(&P->proc.heap,(integer)keyEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)keyEvent->time);
      ret = equal(P,&tmp,&a[3]);
    }

    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)keyEvent->x);
      ret = equal(P,&tmp,&a[4]);
    }

    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)keyEvent->y);
      ret = equal(P,&tmp,&a[5]);
    }

    // Keycode number
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)keySym);
      ret = equal(P,&tmp,&a[6]);
    }

    // Key mask
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)(keyEvent->state));
      ret = equal(P,&tmp,&a[7]);
    }

    // Key stroke string
    if(ret==Ok){
      tmp = allocateCString(&P->proc.heap,stroke);
      ret = equal(P,&tmp,&a[10]);
    }
    
    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case ButtonPress:{
    XButtonEvent *btnEvent = (XButtonEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    // Time in milliseconds of the event
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->time);
      ret = equal(P,&tmp,&a[3]);
    }

    // Window-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->x);
      ret = equal(P,&tmp,&a[4]);
    }

    // Window-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->y);
      ret = equal(P,&tmp,&a[5]);
    }

    // Root-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->x_root);
      ret = equal(P,&tmp,&a[6]);
    }

    // Root-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->y_root);
      ret = equal(P,&tmp,&a[7]);
    }

    // Key mask
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->state);
      ret = equal(P,&tmp,&a[8]);
    }

    // Button number
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->button);
      ret = equal(P,&tmp,&a[9]);
    }

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case ButtonRelease:{
    XButtonEvent *btnEvent = (XButtonEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    // Time in milliseconds of the event
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->time);
      ret = equal(P,&tmp,&a[3]);
    }

    // Window-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->x);
      ret = equal(P,&tmp,&a[4]);
    }


    // Window-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->y);
      ret = equal(P,&tmp,&a[5]);
    }

    // Root-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->x_root);
      ret = equal(P,&tmp,&a[6]);
    }

    // Root-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->y_root);
      ret = equal(P,&tmp,&a[7]);
    }

    // Key mask
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->state);
      ret = equal(P,&tmp,&a[8]);
    }

    // Button number
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)btnEvent->button);
      ret = equal(P,&tmp,&a[9]);
    }

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case MotionNotify:{
    XMotionEvent *mtnEvent = (XMotionEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)mtnEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    // Time in milliseconds of the event
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)mtnEvent->time);
      ret = equal(P,&tmp,&a[3]);
    }

    // Window-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)mtnEvent->x);
      ret = equal(P,&tmp,&a[4]);
    }

    // Window-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)mtnEvent->y);
      ret = equal(P,&tmp,&a[5]);
    }

    // Root-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)mtnEvent->x_root);
      ret = equal(P,&tmp,&a[6]);
    }

    // Root-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)mtnEvent->y_root);
      ret = equal(P,&tmp,&a[7]);
    }

    // Key mask
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)mtnEvent->state);
      ret = equal(P,&tmp,&a[8]);
    }

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case EnterNotify:{
    XCrossingEvent *crxEvent = (XCrossingEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    // Time in milliseconds of the event
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->time);
      ret = equal(P,&tmp,&a[3]);
    }

    // Window-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->x);
      ret = equal(P,&tmp,&a[4]);
    }

    // Window-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->y);
      ret = equal(P,&tmp,&a[5]);
    }

    // Root-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->x_root);
      ret = equal(P,&tmp,&a[6]);
    }

    // Root-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->y_root);
      ret = equal(P,&tmp,&a[7]);
    }

    // Key mask
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->state);
      ret = equal(P,&tmp,&a[8]);
    }

    // Focus
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->focus);
      ret = equal(P,&tmp,&a[9]);
    }

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case LeaveNotify:{
    XCrossingEvent *crxEvent = (XCrossingEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    // Time in milliseconds of the event
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->time);
      ret = equal(P,&tmp,&a[3]);
    }

    // Window-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->x);
      ret = equal(P,&tmp,&a[4]);
    }

    // Window-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->y);
      ret = equal(P,&tmp,&a[5]);
    }

    // Root-relative x-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->x_root);
      ret = equal(P,&tmp,&a[6]);
    }

    // Root-relative y-coord
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->y_root);
      ret = equal(P,&tmp,&a[7]);
    }

    // Key mask
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->state);
      ret = equal(P,&tmp,&a[8]);
    }

    // Focus
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)crxEvent->focus);
      ret = equal(P,&tmp,&a[9]);
    }

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case FocusIn:{
    XFocusChangeEvent *fcsEvent = (XFocusChangeEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)fcsEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case FocusOut:{
    XFocusChangeEvent *fcsEvent = (XFocusChangeEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)fcsEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

    //  case KeymapNotify:
  case Expose:{
    XExposeEvent *exEvent = (XExposeEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)exEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    // x-coord of exposed rectangle
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)exEvent->x);
      ret = equal(P,&tmp,&a[3]);
    }
      
    // y-coord of exposed rectangle
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)exEvent->y);
      ret = equal(P,&tmp,&a[4]);
    }
      
    // Width of exposed rectangle
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)exEvent->width);
      ret = equal(P,&tmp,&a[5]);
    }
      
    // Height of exposed rectangle
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)exEvent->height);
      ret = equal(P,&tmp,&a[6]);
    }
      
    // Remaining expose event count
    if(ret==Ok){
      tmp = allocateInteger(&P->proc.heap,(integer)exEvent->count);
      ret = equal(P,&tmp,&a[7]);
    }

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }
    //  case GraphicsExpose:
    //  case NoExpose:
    //  case VisibilityNotify:

  case CreateNotify:{
    XCreateWindowEvent *crEvent = (XCreateWindowEvent*)&event;
    if(!crEvent->override_redirect){
      ptrI tmp = allocateInteger(&P->proc.heap,(integer)crEvent->window);
      gcAddRoot(&P->proc.heap,&tmp);
      retCode ret = equal(P,&eventCode,&a[1]);

      // Event Window
      if(ret==Ok)
	ret = equal(P,&tmp,&a[2]);

      // x-coord of window location
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)crEvent->x);
	ret = equal(P,&tmp,&a[3]);
      }
      
      // y-coord of window location
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)crEvent->y);
	ret = equal(P,&tmp,&a[4]);
      }
      
      // Width of window
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)crEvent->width);
	ret = equal(P,&tmp,&a[5]);
      }
      
      // Height of window
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)crEvent->height);
	ret = equal(P,&tmp,&a[6]);
      }

      // Window border
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)crEvent->border_width);
	ret = equal(P,&tmp,&a[7]);
      }
      
      gcRemoveRoot(&P->proc.heap,root);
      return ret;
    }
    else
      goto again;
  }

  case DestroyNotify:{
    XDestroyWindowEvent *dsEvent = (XDestroyWindowEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)dsEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case UnmapNotify:{
    XUnmapEvent *mapEvent = (XUnmapEvent*)&event;
    ptrI tmp = allocateInteger(&P->proc.heap,(integer)mapEvent->window);
    gcAddRoot(&P->proc.heap,&tmp);
    retCode ret = equal(P,&eventCode,&a[1]);

    // Event Window
    if(ret==Ok)
      ret = equal(P,&tmp,&a[2]);

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }

  case MapNotify:{
    XMapEvent *mapEvent = (XMapEvent*)&event;
    if(!mapEvent->override_redirect){
      ptrI tmp = allocateInteger(&P->proc.heap,(integer)mapEvent->window);
      gcAddRoot(&P->proc.heap,&tmp);
      retCode ret = equal(P,&eventCode,&a[1]);

      // Event Window
      if(ret==Ok)
	ret = equal(P,&tmp,&a[2]);
      
      gcRemoveRoot(&P->proc.heap,root);
      return ret;
    }
    else
      goto again;
  }
    //  case MapRequest:

  case ReparentNotify:{
    XReparentEvent *rpEvent = (XReparentEvent*)&event;

    if(!rpEvent->override_redirect){
      ptrI tmp = allocateInteger(&P->proc.heap,(integer)rpEvent->window);
      gcAddRoot(&P->proc.heap,&tmp);
      retCode ret = equal(P,&eventCode,&a[1]);

      // Event Window
      if(ret==Ok)
	ret = equal(P,&tmp,&a[2]);

      if(ret==Ok){
	// Parent Window
	if(rpEvent->parent!=RootWindow(display,screen_num))
	  tmp = allocateInteger(&P->proc.heap,(integer)rpEvent->parent);
	else
	  tmp = zero;
	ret = equal(P,&tmp,&a[3]);
      }

      // x-coord of window location
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)rpEvent->x);
	ret = equal(P,&tmp,&a[4]);
      }
      
      // y-coord of window location
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)rpEvent->y);
	ret = equal(P,&tmp,&a[5]);
      }

      gcRemoveRoot(&P->proc.heap,root);
      return ret;
    }
    else
      goto again;
  }

  case ConfigureNotify:{
    XConfigureEvent *cnfEvent = (XConfigureEvent*)&event;

    if(!cnfEvent->override_redirect){
      ptrI tmp = allocateInteger(&P->proc.heap,(integer)cnfEvent->window);
      gcAddRoot(&P->proc.heap,&tmp);
      retCode ret = equal(P,&eventCode,&a[1]);

      // Event Window
      if(ret==Ok)
	ret = equal(P,&tmp,&a[2]);

      // x-coord of window location
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)cnfEvent->x);
	ret = equal(P,&tmp,&a[3]);
      }
      
      // y-coord of window location
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)cnfEvent->y);
	ret = equal(P,&tmp,&a[4]);
      }
      
      // Width of window
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)cnfEvent->width);
	ret = equal(P,&tmp,&a[5]);
      }
      
      // Height of window
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)cnfEvent->height);
	ret = equal(P,&tmp,&a[6]);
      }

      // Window border
      if(ret==Ok){
	tmp = allocateInteger(&P->proc.heap,(integer)cnfEvent->border_width);
	ret = equal(P,&tmp,&a[7]);
      }
      
      gcRemoveRoot(&P->proc.heap,root);
      return ret;
    }
    else
      goto again;
  }
  case ConfigureRequest:
  case GravityNotify:
  case ResizeRequest:
  case CirculateNotify:
  case CirculateRequest:
  case PropertyNotify:
  case SelectionClear:
  case SelectionRequest:
  case SelectionNotify:
  case ColormapNotify:
  case ClientMessage:
  case MappingNotify:
  default:
#ifdef XTRACE
  if(traceX)
    outMsg(logFile,"%s event %d ignored\n%_",event_name[event.type],
	   event.xany.serial);
#endif
    goto again;
  }
}

retCode x_eventMask(processPo P,ptrPo a)
{
  ptrI w = deRefI(&a[1]);		/* The window ID to map */
  ptrI r = deRefI(&a[2]);		/* The event mask */
  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(w)||isvar(r))
    return liberror(P,"xEventMask",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xEventMask",eNOX);
  else{
    Window win = (Window)integerVal(intV(w));
    unsigned int Mask = integerVal(intV(r));

    XSelectInput(display,win,Mask);
    return Ok;
  }
}
