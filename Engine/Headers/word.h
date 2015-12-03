/* 
   Main memory layout definitions for the Go! engine
   (c) 2000-2007 F.G.McCabe

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

#ifndef _ENGINE_WORD_H_
#define _ENGINE_WORD_H_

#include "logical.h"		/* import a definition of true and false */
#include "integer.h"
#include "number.h"
#include "retcode.h"
#include <math.h>
#include <assert.h>
#include <stdlib.h>

/* Standard constant definitions */

/* This is used for many internal fixed char buffers */
#ifndef MAX_SYMB_LEN
#define MAX_SYMB_LEN 1024
#endif

/* This is used for some internal stack buffers */
#ifndef MAX_DEPTH
#define MAX_DEPTH 512
#endif

/* This is used for some internal tables */
#ifndef MAX_TABLE
#define MAX_TABLE 2048
#endif

/* This is used for message buffers */
#ifndef MAX_MSG_LEN
#define MAX_MSG_LEN 1024
#endif


/* BE VERY CAREFUL if you change this .... */
/* Definition of a cell/pointer for the Go! run-time engine
 * The value pointed at is preceded by a descriptor word which contains
 * a tag and possible attribute value
 */

/* As with nearly every Prolog system out there, the Go! engine uses a tagged 
   pointer architecture. However, this one uses both tagged pointer and tagged 
   cell. This is to minimize the in-pointer tags (to 2 bits) and to maximize 
   potential for expansion.

   The two pointer bits are in the bottom (lsb) of the pointer, they tell us:

   00 - a variable, or a variable-variable reference
   01 - object, pointer is a pointer to a tagged structure
   10 - forward pointer, GC has moved the object somewhere
*/

/* The idea is this:
   A ptrPo points to a ptrI, which is a pointer with a tag in the lower 2 bits.
   A stripped ptrI becomes an objPo, which is normally a pointer to a structure.

   The first word in the structure must have the tag set to *sign*. This is
   the sentinel for the beginning of all structures.

   The special case is variable, which may point *into* structure as well as
   to the beginning of the structure. In the case that a variable points
   into a structure, it is the result of a variable-variable binding.

   The first word in the structure is actually a pointer to a class structure.
   This class structure is used to decode the following sequence of words.

   There are two cases of structure:
   1. A term class constructor which consists of N tagged pointer words. The arity
   N comes from the class structure.
   2. A special object. The class structure has functions that define the layout
   of the object. Examples include symbols, integer, float, char, code, dynamic objects

   A ptrI is defined to be an integer which is long enough to hold a pointer.
   The assumption that we make is that a pointer is at least 4 bytes.
*/
typedef PTRINT ptrI,*ptrPo;	/* ptrInt normally generated during config */
typedef struct _general_record_ *objPo; /* a core pointer */

typedef struct _class_record_ *clssPo;
typedef struct _program_record_ *programPo;
typedef struct _special_class_ *specialClassPo;

typedef struct _heap_rec_ *heapPo;

#define TAG_SHFT 2
#define TAG_MASK ((1<<TAG_SHFT)-1)
#define PTR_MASK (~TAG_MASK)

typedef enum {
  varTg = 0,                            // A variable pointer
  objTg = 1,                            // An object
  fwdTg = 2				// A forwarded structure
} ptrTg;

#define ptg(p) ((ptrTg)(((ptrI)(p))&TAG_MASK))
#define isvar(p) (ptg(p)==varTg)
#define isobj(p) (ptg(p)==objTg)
#define isfwd(p) (ptg(p)==fwdTg)

#define ptrP(p,t)  (((ptrI)(p))|(t))
#define varP(p)  ptrP(p,varTg)
#define objP(p)  ptrP(p,objTg)
#define fwdP(p)  ptrP(p,fwdTg)

#define objV(p) ((objPo)(((ptrI)(p))&PTR_MASK))

typedef struct _general_record_ {
  ptrI class;				// basic object has a class
  ptrI args[ZEROARRAYSIZE];
} generalRec;

typedef enum {
  smaller,                      /* One item is smaller than another */
  same,                         /* Two items are the same */
  bigger,                       /* One it bigger than the other */
  incomparible                  /* Incomparible */
} comparison;           

// Some typedefs to help with working with classes

typedef long (*classSizeFun)(specialClassPo class,objPo o);
typedef uinteger (*classHashFun)(specialClassPo class,objPo o);
typedef comparison (*classCompFun)(specialClassPo class,objPo o1,objPo o2);
typedef retCode (*classOutFun)(specialClassPo class,ioPo out,objPo o1);

typedef retCode (*specialHelperFun)(ptrPo arg,void *c);
typedef retCode (*classScanFun)(specialClassPo class,specialHelperFun helper,void *c,objPo o);
typedef objPo (*classCpyFun)(specialClassPo class,objPo dst,objPo src);

// The sign of a general_record points to a class object ...
typedef struct _class_record_ {
  ptrI class;				/* == classClass */
  uinteger hash;			/* the hash code for this class */
  long arity;				/* how many term args */
  uniChar name[ZEROARRAYSIZE];		/* the class's print name */
} clssRec;


// Special classes mostly refer to special system objects
typedef struct _special_class_ {
  ptrI class;				/* == specialClass */
  classSizeFun sizeFun;			/* Function to compute size of object */
  classCompFun compFun;			/* Function to compare two values */
  classOutFun outFun;			/* Function to write a value */
  classCpyFun copyFun;			/* Function to copy special object */
  classScanFun scanFun;			/* Function to scan object */
  classHashFun hashFun;			/* Function to compute hash function */
  ptrI program;				/* Program that responds to this type */
  uniChar name[ZEROARRAYSIZE];		/* the class's print name */
} specialClassRec;

#define PTRSZE sizeof(ptrI)

#define ALIGNPTR(count,size) (((count+size-1)/size)*(size))
#define CellCount(size) (ALIGNPTR(size,PTRSZE)/PTRSZE)
#define ALIGNED(ptr,size) (((((ptrI)ptr)+size-1)/size)*(size)==(ptrI)ptr)

extern ptrI classClass, specialClass, programClass;

static inline logical isClass(ptrI p)
{
  return ((clssPo)objV(p))->class==classClass;
}

static inline logical isSpecialClass(clssPo cl)
{
  return cl->class==specialClass;
}

static inline logical IsSpecialClass(ptrI p)
{
  return isSpecialClass((clssPo)objV(p));
}

static inline logical isTermClass(objPo cl)
{
  return cl->class==classClass;
}

static inline logical IsTermClass(ptrI p)
{
  return isTermClass(objV(p));
}

/* static inline logical isObjct(objPo o) */
/* { */
/*   return isClass(o->class); */
/* } */

#define isObjct(o) (isClass((o)->class))

static inline logical isObjBase(ptrPo p)
{
  if(isobj(*p)){
    objPo o = objV(*p);

    return o->class==classClass;
  }
  else
    return False;
}

static inline logical isSpecialObject(objPo o)
{
  return IsSpecialClass(o->class);
}

static inline specialClassPo sClassOf(objPo o)
{
  ptrI class = o->class;
  assert(IsSpecialClass(class));
  return (specialClassPo)objV(class);
}

/*
 * Find the class of an object
 */
static inline clssPo classOf(objPo p)
{
  return (clssPo)objV(p->class);
}

static inline clssPo ClassOf(ptrI X)
{
  return classOf(objV(X));
}

#if 0
static inline logical hasClass(objPo p,ptrI class)
{
  return p->class==class;
}
#endif

#define hasClass(p,cl) ((p)->class==(cl))

#if 0
static inline logical HasClass(ptrI x,ptrI class)
{
  return isobj(x) && hasClass(objV(x),class);
}
#endif

#define HasClass(x,cl) ((objV(x))->class==(cl))

static inline long objectSize(objPo p)
{
  clssPo class = classOf(p);

  assert(class->class==classClass);

  return class->arity+1;
}

static inline long specialSize(objPo p)
{
  specialClassPo spClass = (specialClassPo)objV(p->class);

  assert(spClass->class==specialClass);

  return spClass->sizeFun(spClass,p);
}

static inline ptrI specialProgram(objPo p)
{
  specialClassPo spClass = (specialClassPo)objV(p->class);

  assert(spClass->class==specialClass);

  return spClass->program;
}

static inline long objectArity(objPo o)
{
  clssPo class = classOf(o);

  return class->arity;
}

#if 0
static inline long classArity(clssPo class)
{
  return class->arity;
}
#endif
#define classArity(cl) ((cl)->arity)


#if 0
static inline ptrPo objectArgs(objPo o)
{
  assert(isObjct(o));

  return o->args;
}
#else
#define objectArgs(o) (((objPo)(o))->args)
#endif

static inline ptrPo nthArg(objPo o,long ix)
{
  assert(ix>=0 && ix<objectArity(o));

  return objectArgs(o)+ix;
}

static inline void updateArg(objPo o,long ix,ptrI val)
{
  assert(ix>=0 && ix<objectArity(o));

  ptrPo args = objectArgs(o);

  args[ix] = val;
}
  
static inline uniChar *objectClassName(objPo p)
{
  clssPo class = classOf(p);
  return class->name;
}

static inline uniChar *className(clssPo class)
{
  return class->name;
}

static inline uinteger objectHash(objPo p)
{
  clssPo class = classOf(p);
  return class->hash;
}

extern logical IsBinOp(ptrPo p,ptrI key,ptrPo a1,ptrPo a2);

extern ptrI newClassDf(const char *name,long arity);
extern ptrI newClassDef(const uniChar *fun,long arity);
extern ptrI newSpecialClass(const char *name,
			    classSizeFun sizeFun, classCompFun compFun,
			    classOutFun outFun, classCpyFun copyFun,
			    classScanFun scanFun, classHashFun hashFun);
extern void initClass(void);
extern void standardClasses(void);
extern void installClass(clssPo class);
extern ptrI classPresent(uniChar *name);

#include "heap.h"
#include "list.h"
#include "vars.h"
#include "floats.h"
#include "symbols.h"
#include "char.h"
#include "code.h"
#include "str.h"

#endif
