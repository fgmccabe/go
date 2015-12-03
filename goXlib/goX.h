/* 
  Header file for the Xlib interface library
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
#ifndef GO_X_H
#define GO_X_H

#include <X11/Xlib.h>			/* Access the Xlib stuff */
#include <X11/Xutil.h>
#include <X11/Xos.h>			/* Some OS independency */

extern Display *getDisplay(void);
extern Screen *screen;
extern int screen_num;

extern void xInitFonts(void);

#ifdef XTRACE
extern logical traceX;
#endif

#endif
