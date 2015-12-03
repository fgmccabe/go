/* 
  Display management for the Xlib
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

static pthread_mutex_t xlibMutex = PTHREAD_MUTEX_INITIALIZER;

static Display *display = NULL;
Screen *screen;
int screen_num;

#ifdef XTRACE
logical traceX = False;
#endif

Display *getDisplay(void)
{
  pthread_mutex_lock(&xlibMutex);
  Display *disp = display;
  pthread_mutex_unlock(&xlibMutex);
  return disp;
}

retCode x_openDisplay(processPo P,ptrPo a)
{
  uniChar displayName[MAX_MSG_LEN];
  char display_name[MAX_MSG_LEN];
  ptrI d = deRefI(&a[1]);
  if(isvar(d)||isGroundString(&d)!=Ok)
    return liberror(P,"xOpenDisplay",eSTRNEEDD);

  pthread_mutex_lock(&xlibMutex);
  
  if(XInitThreads()){
    String2Uni(&d,displayName,NumberOf(displayName));
    _utf(displayName,(unsigned char*)display_name,NumberOf(display_name));

    if((display=XOpenDisplay(display_name))==NULL){
      pthread_mutex_unlock(&xlibMutex);
      return liberror(P,"xOpenDisplay",eCONNECT);
    }
    else{
      screen_num = DefaultScreen(display);
      screen = DefaultScreenOfDisplay(display);

      xInitFonts();

      pthread_mutex_unlock(&xlibMutex);

      return Ok;
    }
  }
  else
    return liberror(P,"xOpenDisplay",eCONNECT);
}

retCode x_displayWidth(processPo P,ptrPo a)
{
  if(display==NULL)
    return liberror(P,"xDisplayWidth",eNOX);
  else{
    ptrI width = allocateInteger(&P->proc.heap,XDisplayWidth(display,screen_num));
    return equal(P,&a[1],&width);
  }
}

retCode x_displayHeight(processPo P,ptrPo a)
{
  if(display==NULL)
    return liberror(P,"xDisplayHeight",eNOX);
  else{
    ptrI h = allocateInteger(&P->proc.heap,XDisplayHeight(display,screen_num));
    return equal(P,&a[1],&h);
  }
}

retCode x_displayWidthMM(processPo P,ptrPo a)
{
  if(display==NULL)
    return liberror(P,"xDisplayWidthMM",eNOX);
  else{
    ptrI width = allocateInteger(&P->proc.heap,XDisplayWidthMM(display,screen_num));
    return equal(P,&a[1],&width);
  }
}

retCode x_displayHeightMM(processPo P,ptrPo a)
{
  if(display==NULL)
    return liberror(P,"xDisplayHeightMM",eNOX);
  else{
    ptrI h = allocateInteger(&P->proc.heap,XDisplayHeightMM(display,screen_num));
    return equal(P,&a[1],&h);
  }
}

retCode x_rootWindow(processPo P,ptrPo a)
{
  if(display==NULL)
    return liberror(P,"xRootWindow",eNOX);
  else{
    ptrI h = allocateInteger(&P->proc.heap,RootWindow(display,screen_num));
    return equal(P,&a[1],&h);
  }
}


