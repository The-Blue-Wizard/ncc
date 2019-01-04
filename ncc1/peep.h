/* Copyright (c) 2018 Charles E. Youse (charles@gnuless.org). 
   All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

/* an array of 'struct peep_match' is a template to match an 
   instruction sequence, terminated with an 'opcode' of I_NONE.

   note that if sequences get any longer than 8 instructions, 
   the encoding of PMO_SAME_AS() will have to be changed. */

#define PMI_CCS_UNUSED      0x00000001      /* CCs (if any) from this insn aren't used */

#define PMO_REG             0x00000001      /* is E_REG */
#define PMO_DEAD            0x00000002      /* E_REG is dead */
#define PMO_DEF             0x00000004      /* E_REG is DEFd in this insn */
#define PMO_USE             0x00000008      /* E_REG is USEd in this insn */
#define PMO_NOTDEF          0x00000010      /* E_REG is not DEFd in this insn */
#define PMO_NOTUSE          0x00000020      /* E_REG is not USEd in this insn */
#define PMO_UNALIASED       0x00000040      /* E_REG is not aliased */
#define PMO_CON             0x00000080      /* is E_CON */
#define PMO_VALUE           0x00000100      /* u.con.i == value */

    /* operand must be the same as operand 'j' of insn 'i' in the sequence.
       these can only be used to refer to the current or previous insns in
       the sequence, because of the way peep_match() works. */

#define PMO_SAME_AS(i,j)    (PMO_SAME | PMO_SAME_PUT_INSN(i) | PMO_SAME_PUT_OPERAND(i))

    /* encoding/decoding helpers for PMO_SAME_AS(). for now, pmo[31] = flag,
       pmo[30:29] = operand index, pmo[28:26] = insn index. note that if sequences 
       get any longer than 8 instructions, the encoding will have to be changed. */

#define PMO_SAME                    0x80000000 

#define PMO_SAME_PUT_INSN(i)        ((i) << 26)
#define PMO_SAME_GET_INSN(i)        (((i) >> 26) & 0x07)
#define PMO_SAME_PUT_OPERAND(i)     ((i) << 29)
#define PMO_SAME_GET_OPERAND(i)     (((i) >> 29) & 0x03)

struct peep_match
{
    int opcode;     /* I_* from block.h */
    int pmis;       /* PMI_*, instruction-level flags */

    struct 
    {
        int  ts;        /* T_*, bitset of applicable operand types */
        int  pmos;      /* PMO_*, operand-level flags */
        long value;     /* for PMO_VALUE */
    } operand[NR_INSN_OPERANDS];

    /* 'tmp' is actually used by the matching code only. it should not
        be initialized and has no useful contents outside peep_match(). */

    struct insn * tmp;
};

