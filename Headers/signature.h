#ifndef _GO_TYPE_SIG_H_
#define _GO_TYPE_SIG_H_

/* Type signatures for Go! program type */
typedef enum{
  integer_sig='i',                      /* An integer */
  float_sig='f',                        /* A float */
  number_sig='N',                       /* Any number */
  symbol_sig='s',                       /* Symbol */
  char_sig='c',                         /* Character */
  string_sig='S',                       /* String */
  logical_sig='l',                      /* Logical value */
  var_sig='$',                          /* Type variable */
  top_sig='A',                      /* Top value -- of unknown type */
  opaque_sig='O',                /* An opaque value, has an owner and a value */
  type_sig='t',                         /* A type variable */
  
  /* Compound type signatures */
  list_sig='L',                         /* List pair -- NULL = nil */
  tuple_sig='T',                        /* Tuple - followed by length byte */
  forall_sig=':',                       /* universally quantified formula */
    
/* signatures for user-defined types */
  poly_sig = 'U',                       /* polymorphic user type */
    
/* Code signatures */
  funct_sig='F',                        /* Function object signature */
  proc_sig='P',                         /* Procedure signature */
  action_sig='p',                       /* Process rule signature */
  grammar_sig='G',                      /* Grammar rule signature */

  face_sig='I',                         /* class interface type specification */
  
// Used internally only
  con_sig = 'C',                        /* constructor function */
  enu_sig = 'E',                        /* enumerated symbol */
} goTypeSig;

/* First Go! version */
#define SIGNATURE 0x01030507L	/* code signature */
#define SIGNBSWAP 0x03010705L	/* signature when we must swap bytes not words */
#define SIGNWSWAP 0x05070103L	/* signature to sap words only */
#define SIGNBWSWP 0x07050301L	/* when we have to swap words and bytes */

#endif
