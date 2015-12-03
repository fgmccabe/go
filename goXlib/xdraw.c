/* 
  Basic drawing primitives
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

#define MAX_POINT 128			/* Buffer size for points */

retCode x_drawPoint(processPo P,ptrPo a)
{
  ptrI win = deRefI(&a[1]);		/* The window to draw into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI x = deRefI(&a[3]);		/* X-Coord */
  ptrI y = deRefI(&a[4]);		/* Y-Coord */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(win)||isvar(g)||isvar(x)||isvar(y))
    return liberror(P,"xDrawPoint",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xDrawPoint",eNOX);
  else{
    int X = (int)integerVal(intV(x));
    int Y = (int)integerVal(intV(y));

    Drawable d = (Drawable)integerVal(intV(win));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    XDrawPoint(display,d,gc,X,Y);
    
    return Ok;
  }
}

retCode x_drawPoints(processPo P,ptrPo a)
{
  ptrI w = deRefI(&a[1]);		/* The window to draw into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI l = deRefI(&a[3]);		/* The set of lines to draw */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(w)||isvar(g)||isvar(l))
    return liberror(P,"xDrawPoints",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xDrawPoints",eNOX);
  else{
    long len = ListLen(l);
    if(len<0)
      return liberror(P,"xDrawPoints",eINSUFARG);

    XPoint buff[MAX_POINT];
    XPoint *points = buff;
    Drawable d = (Drawable)integerVal(intV(w));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    if(len>NumberOf(buff))
      points = (XPoint *)malloc(sizeof(XPoint)*len);

    ptrI xx = l;
    int i = 0;

    while(IsList(xx)){
      ptrI e = deRefI(listHead(listV(xx)));
      ptrI x,y;

      if(!isvar(e) && IsBinOp(&e,commaClass,&x,&y) && !isvar(x) && !isvar(y)){
	points[i].x = (int)integerVal(intV(x));
	points[i].y = (int)integerVal(intV(y));
	i++;
      }
      else{
	if(points!=buff)
	  free(points);
	return liberror(P,"xDrawPoints",eINSUFARG);
      }

      xx = deRefI(listTail(listV(xx)));
    }

    XDrawPoints(display,d,gc,points,len,CoordModeOrigin);
    
    return Ok;
  }
}

retCode x_drawLines(processPo P,ptrPo a)
{
  ptrI w = deRefI(&a[1]);		/* The window to draw into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI l = deRefI(&a[3]);		/* The set of lines to draw */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(w)||isvar(g)||isvar(l))
    return liberror(P,"xDrawLines",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xDrawLines",eNOX);
  else{
    long len = ListLen(l);
    if(len<0)
      return liberror(P,"xDrawLines",eINSUFARG);

    XPoint buff[MAX_POINT];
    XPoint *points = buff;
    Drawable d = (Drawable)integerVal(intV(w));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    if(len>NumberOf(buff))
      points = (XPoint *)malloc(sizeof(XPoint)*len);

    ptrI xx = l;
    int i = 0;

    while(IsList(xx)){
      ptrI e = deRefI(listHead(listV(xx)));
      ptrI x,y;

      if(!isvar(e) && IsBinOp(&e,commaClass,&x,&y) && !isvar(x) && !isvar(y)){
	points[i].x = (int)integerVal(intV(x));
	points[i].y = (int)integerVal(intV(y));
	i++;
      }
      else{
	if(points!=buff)
	  free(points);
	return liberror(P,"xDrawLines",eINSUFARG);
      }

      xx = deRefI(listTail(listV(xx)));
    }

    XDrawLines(display,d,gc,points,len,CoordModeOrigin);
    
    return Ok;
  }
}

retCode x_drawRectangle(processPo P,ptrPo a)
{
  ptrI win = deRefI(&a[1]);		/* The window to draw into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI x = deRefI(&a[3]);		/* X-Coord of top-left */
  ptrI y = deRefI(&a[4]);		/* Y-Coord of top-left */
  ptrI w = deRefI(&a[5]);		/* Width of the rectangle */
  ptrI h = deRefI(&a[6]);		/* Height of the rectangle */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(win)||isvar(g)||isvar(x)||isvar(y)||isvar(w)||isvar(h))
    return liberror(P,"xDrawRectangle",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xDrawRectangle",eNOX);
  else{
    int X = (int)integerVal(intV(x));
    int Y = (int)integerVal(intV(y));
    unsigned int W = (unsigned int)integerVal(intV(w));
    unsigned int H = (unsigned int)integerVal(intV(h));

    Drawable d = (Drawable)integerVal(intV(win));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    XDrawRectangle(display,d,gc,X,Y,W,H);
    
    return Ok;
  }
}

retCode x_fillRectangle(processPo P,ptrPo a)
{
  ptrI win = deRefI(&a[1]);		/* The window to fill into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI x = deRefI(&a[3]);		/* X-Coord of top-left */
  ptrI y = deRefI(&a[4]);		/* Y-Coord of top-left */
  ptrI w = deRefI(&a[5]);		/* Width of the rectangle */
  ptrI h = deRefI(&a[6]);		/* Height of the rectangle */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(win)||isvar(g)||isvar(x)||isvar(y)||isvar(w)||isvar(h))
    return liberror(P,"xFillRectangle",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xFillRectangle",eNOX);
  else{
    int X = (int)integerVal(intV(x));
    int Y = (int)integerVal(intV(y));
    unsigned int W = (unsigned int)integerVal(intV(w));
    unsigned int H = (unsigned int)integerVal(intV(h));

    Drawable d = (Drawable)integerVal(intV(win));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    XFillRectangle(display,d,gc,X,Y,W,H);
    
    return Ok;
  }
}

retCode x_drawOval(processPo P,ptrPo a)
{
  ptrI win = deRefI(&a[1]);		/* The window to draw into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI x = deRefI(&a[3]);		/* X-Coord of top-left */
  ptrI y = deRefI(&a[4]);		/* Y-Coord of top-left */
  ptrI w = deRefI(&a[5]);		/* Width of the oval */
  ptrI h = deRefI(&a[6]);		/* Height of the oval */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(win)||isvar(g)||isvar(x)||isvar(y)||isvar(w)||isvar(h))
    return liberror(P,"xDrawOval",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xDrawOval",eNOX);
  else{
    int X = (int)integerVal(intV(x));
    int Y = (int)integerVal(intV(y));
    unsigned int W = (unsigned int)integerVal(intV(w));
    unsigned int H = (unsigned int)integerVal(intV(h));

    Drawable d = (Drawable)integerVal(intV(win));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    XDrawArc(display,d,gc,X,Y,W,H,0,360*64);
    
    return Ok;
  }
}

retCode x_fillOval(processPo P,ptrPo a)
{
  ptrI win = deRefI(&a[1]);		/* The window to draw into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI x = deRefI(&a[3]);		/* X-Coord of top-left */
  ptrI y = deRefI(&a[4]);		/* Y-Coord of top-left */
  ptrI w = deRefI(&a[5]);		/* Width of the oval */
  ptrI h = deRefI(&a[6]);		/* Height of the oval */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(win)||isvar(g)||isvar(x)||isvar(y)||isvar(w)||isvar(h))
    return liberror(P,"xFillOval",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xFillOval",eNOX);
  else{
    int X = (int)integerVal(intV(x));
    int Y = (int)integerVal(intV(y));
    unsigned int W = (unsigned int)integerVal(intV(w));
    unsigned int H = (unsigned int)integerVal(intV(h));

    Drawable d = (Drawable)integerVal(intV(win));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    XFillArc(display,d,gc,X,Y,W,H,0,360*64);
    
    return Ok;
  }
}

static number skewAngle(number angle,number width,number height)
{
  if(height>0){
    angle = fmod(angle,2*PI);
    number adjust = (angle>=0 && angle<=PI/2.0 ? 0 : 
		     angle>=PI/2.0 && angle<=PI*3/2.0 ? PI :
		     2*PI);
    return atan(tan(angle)*width/height)+adjust;
  }
  else
    return angle;
}

retCode x_drawArc(processPo P,ptrPo a)
{
  ptrI win = deRefI(&a[1]);		/* The window to draw into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI x = deRefI(&a[3]);		/* X-Coord of top-left */
  ptrI y = deRefI(&a[4]);		/* Y-Coord of top-left */
  ptrI w = deRefI(&a[5]);		/* Width of the arc */
  ptrI h = deRefI(&a[6]);		/* Height of the arc */
  ptrI s = deRefI(&a[7]);		/* Start arc angle */
  ptrI e = deRefI(&a[8]);		/* Endt arc angle */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(win)||isvar(g)||isvar(x)||isvar(y)||isvar(w)||isvar(h)||isvar(s)||isvar(e))
    return liberror(P,"xDrawArc",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xDrawArc",eNOX);
  else{
    int X = (int)integerVal(intV(x));
    int Y = (int)integerVal(intV(y));
    unsigned int W = (unsigned int)integerVal(intV(w));
    unsigned int H = (unsigned int)integerVal(intV(h));
    number S = skewAngle(NumberVal(objV(s)),W,H)*PI*64.0/180.0;
    number E = skewAngle(NumberVal(objV(e)),W,H)*PI*64.0/180.0;

    Drawable d = (Drawable)integerVal(intV(win));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    XDrawArc(display,d,gc,X,Y,W,H,S,E);
    
    return Ok;
  }
}

retCode x_fillArc(processPo P,ptrPo a)
{
  ptrI win = deRefI(&a[1]);		/* The window to draw into */
  ptrI g = deRefI(&a[2]);		/* The graphics context */
  ptrI x = deRefI(&a[3]);		/* X-Coord of top-left */
  ptrI y = deRefI(&a[4]);		/* Y-Coord of top-left */
  ptrI w = deRefI(&a[5]);		/* Width of the arc */
  ptrI h = deRefI(&a[6]);		/* Height of the arc */
  ptrI s = deRefI(&a[7]);		/* Start arc angle */
  ptrI e = deRefI(&a[8]);		/* Endt arc angle */

  Display *display = getDisplay();	/* Find out if we are connected */

  if(isvar(win)||isvar(g)||isvar(x)||isvar(y)||isvar(w)||isvar(h)||isvar(s)||isvar(e))
    return liberror(P,"xFillArc",eINSUFARG);
  else if(display==NULL)
    return liberror(P,"xFillArc",eNOX);
  else{
    int X = (int)integerVal(intV(x));
    int Y = (int)integerVal(intV(y));
    unsigned int W = (unsigned int)integerVal(intV(w));
    unsigned int H = (unsigned int)integerVal(intV(h));
    number S = skewAngle(NumberVal(objV(s)),W,H)*PI*64.0/180.0;
    number E = skewAngle(NumberVal(objV(e)),W,H)*PI*64.0/180.0;

    Drawable d = (Drawable)integerVal(intV(win));
    GC gc = (GC)((unsigned long)integerVal(intV(g)));

    XFillArc(display,d,gc,X,Y,W,H,S,E);
    
    return Ok;
  }
}

