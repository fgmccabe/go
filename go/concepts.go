/* A concept graph is a much simpler entity than an OWL ontology, but more
   flexible as a result.  This library implements a dynamic concept graph XML
   loader and permits certain kinds of inference on the graph.  (c) 2007
   F.G. McCabe
 
   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>
 */
go.concepts{
  import go.io.
  import go.xml.
  import go.dynamic.
  import go.hash.
  import go.setlib.
  import go.stdparse.
  import go.showable.

  /*
   * A Concept is a very simple animal: it has a fully qualified name, a display name and it can be serialized to XML.
   */

  Concept <~ xmlable.		  	-- Concepts have an XML representation
  Concept <~ { 
	fQname:[]=>symbol.			-- The fully qualified name
	display:[]=>string.
      }.

  /* A triple is a pair of concepts and a relationship between them. */

  Triple <~ xmlable.			-- Triples have an XML representation
  Triple <~ {
	domain:[]=>Concept.
	range:[]=>Concept.
      }.

}

