/* Access the opcodes of the Go machine
 * (c) 2000 F.G.McCabe
 * all rights reserved
 **/
#ifndef _OPCODES_H_
#define _OPCODES_H_

#undef instruction
#define instruction(mnem,op,A1,A2,cmnt) mnem=op,

typedef enum {
#include "instructions.h"	/* Pick up the instructions specification */
  illegalOp } opCode;

#undef instruction

/* All these assume a 32-bit word; anything else must be re-engineered */

#define op_mask  0x000000ff	/* Mask for the opcode of an instruction */
#define vl_mask  0xffffff00	/* Mask for the value field */
#define vl_l_mask 0x0000ff00	/* Low byte for the value field */
#define vl_m_mask 0x00ff0000	/* Middle byte for the value field */
#define vl_h_mask 0xff000000	/* High byte for the value field */
#define vl_o_mask 0x00ffff00	/* Label offset operand */

typedef WORD32 insWord;	/* An instruction word */
typedef insWord *insPo;	/* A program counter value */

#define op_cde(c) ((opCode)(c&op_mask))	/* construct an opcode from a number */

#define op_l_val(v) ((((unsigned)(v))&vl_l_mask)>>8) /* Get low-order operand */
#define op_sl_val(v) ((long)((v)<<16>>24)) /* Get signed low-order operand */
#define op_m_val(v) ((((unsigned)(v))&vl_m_mask)>>16) /* Get middle order operand */
#define op_sm_val(v) ((long)((v)<<8>>24)) /* Get signed middle operand */
#define op_h_val(v) (((unsigned)(v))>>24) /* Get hi-order operand */
#define op_sh_val(v) ((long)((v)>>24)) /* Get signed hi-order operand */

#define op_o_val(v) ((((unsigned)(v))&vl_o_mask)>>8) /* Get offset operand */
#define op_so_val(v) ((long)((v)<<8>>16)) /* Get signed low/middle */
#define op_ll_val(v) ((long)((v)>>8)) /* get signed long offset */

typedef enum {
 nOp,                                   // No operand
 iAh,                          // input argument register in upper slot (0..255)
 oAh,                         // output argument register in upper slot (0..255)
 iAm,                         // input argument register in middle slot (0..255)
 oAm,                        // output argument register in middle slot (0..255)
 iAl,                                   // input argument register in lower slot
 oAl,                                   // output argument register in lower slot
 iLh,					// input local variable offset (0..255)
 iLm,					// input local variable offset (0..255)
 iLl,					// input local variable offset (0..255)
 iLc,                           // input local variable offset (0..65535)
 oLh,                           // output local variable offset  (0..255)
 oLm,                           // output local variable offset  (0..255)
 oLl,                           // output local variable offset  (0..255)
 oLc,                           // output local variable offset  (0..65535)
 iSt,                           // input at current structure pointer
 oSt,                           // output to current structure pointer
 uAr,                           // Arity in upper slot
 oAr,                           // Resulting arity in upper slot
 uLt,                           // small literal in upper slot (-128..127)
 Lt,                            // 16bit literal (-32768..32767)
 vSz,           		// Size of local variable vector
 lSz,                           // trim local variable vector
 cSz,           		// Structure size
 Es,                            // escape code (0..65535)
 pcr,                           // program counter relative offset (-32768..32767)
 pcl,                           // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
 ltl                            // literal number (0..65535)
} opAndSpec;                    // Specification code for an operand

#define oprnd_msk 0xff

#define instr(c) (c)
#define instrd(c,o1) (c|((o1&0xffff)<<8))
#define instrl(c,o1) (c|((o1&oprnd_msk)<<8))
#define instrm(c,o1) (c|((o1&oprnd_msk)<<16))
#define instrh(c,o1) (c|((o1&oprnd_msk)<<24))
#define instrhl(c,o1,o2) (c|((o1&oprnd_msk)<<24)|((o2&oprnd_msk)<<8))
#define instrhm(c,o1,o2) (c|((o1&oprnd_msk)<<24)|((o2&oprnd_msk)<<16))
#define instrml(c,o1,o2) (c|((o1&oprnd_msk)<<16)|((o2&oprnd_msk)<<8))
#define instrhml(c,o1,o2,o3) (c|((o1&oprnd_msk)<<24)|((o2&oprnd_msk)<<16)|(o3&oprnd_msk)<<8)
#define instrb(c,o1) (c|(o1&0xffff)<<8)
#define instrhb(c,o1,o2) (c|((o1&oprnd_msk)<<24)|(o2&0xffff)<<8)

#endif

