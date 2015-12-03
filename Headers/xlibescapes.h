/* 
  This defines the escapes that access the Xlib interface
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
#define colorType "U'graph.xlib#color'\0"
#define eventType "U'graph.xlib#eventType'\0"
#define gcOptionType "U'graph.xlib#gcOption'\0"


  escape("xOpenDisplay",x_openDisplay,400,True,False,"p\1+S","open connection to X server")
  escape("xDisplayWidth",x_displayWidth,401,True,False,"F\0i","width of display")
  escape("xDisplayHeight",x_displayHeight,402,True,False,"F\0i","height of display")
  escape("xRootWindow",x_rootWindow,403,True,False,"F\0i","identity of root window")

  escape("xCreateWindow",x_createWindow,410,True,False,"F\5+i+i+i+i+ii","create a window")

  escape("xTopLevelWindow",x_topLevelWindow,411,True,False,"F\6+i+i+i+i+i+Si","create a window")

  escape("xMapWindow",x_mapWindow,412,True,False,"p\2+i+l","map a window")
  escape("xUnmapWindow",x_unmapWindow,413,True,False,"p\1+i","unmap a window")

  escape("xNextEvent",x_nextEvent,420,True,False,"p\012-i-i-i-i-i-i-i-i-i-S","Wait for next event")

  escape("xEventMask",x_eventMask,421,True,False,"p\2+i+L"eventType,"set the event mask")

  escape("xRGBColor",x_rgbColor,440,True,False,"F\3+i+i+ii","compute a color value")
  escape("xColor",x_Color,441,True,False,"F\4+S-i-i-ii","compute a color value")

  escape("xCreateGC",x_createGC,450,True,False,"F\1+L"gcOptionType"i","construct a graphics context")

  escape("xSetGC",x_setGC,451,True,False,"p\2+i+L"gcOptionType,"adjust a graphics context")

  escape("xDestroyGC",x_destroyGC,452,True,False,"p\1+i","free a graphics context")

  escape("xDrawPoint",x_drawPoint,460,True,False,"p\4+i+i+i+i",
	 "primitive to draw a point")
  escape("xDrawPoints",x_drawPoints,461,True,False,"p\3+i+i+L"tplType"ii",
	 "primitive to draw a sequence of points")
  escape("xDrawLines",x_drawLines,462,True,False,"p\3+i+i+L"tplType"ii",
	 "primitive to draw a joined up sequence of lines")
  escape("xDrawRectangle",x_drawRectangle,463,True,False,"p\6+i+i+i+i+i+i",
	 "primitive to draw rectangle")
  escape("xFillRectangle",x_fillRectangle,464,True,False,"p\6+i+i+i+i+i+i",
	 "primitive to draw filled rectangle")
  escape("xDrawOval",x_drawOval,465,True,False,"p\6+i+i+i+i+i+i",
	 "primitive to draw oval")
  escape("xFillOval",x_fillOval,466,True,False,"p\6+i+i+i+i+i+i",
	 "primitive to draw filled arc")
  escape("xDrawArc",x_drawArc,467,True,False,"p\010+i+i+i+i+i+i+f+f",
	 "primitive to draw arc")
  escape("xFillArc",x_fillArc,468,True,False,"p\010+i+i+i+i+i+i+f+f",
	 "primitive to draw filled arc")
  escape("xDrawString",x_drawString,469,True,False,"p\5+i+i+f+f+S",
	 "primitive to draw text string")
  escape("xStringExtent",x_stringExtent,470,True,False,"p\5+i+S-f-f-f",
	 "primitive to compute graphical size of text string")
  escape("xLoadFont",x_loadFont,471,True,False,"F\1+Si",
	 "load a font into the server")
  escape("xQueryFont",x_queryFont,472,True,False,"p\3+i-f-f",
	 "find out font's ascent and descent")
  escape("xCharInfo",x_charInfo,473,True,False,"p\07+i+c-f-f-f-f-f",
	 "find out a character's measurements")

