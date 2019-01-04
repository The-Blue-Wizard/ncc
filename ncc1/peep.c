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

#include "ncc1.h"

/* return non-zero if 't1' and 't2' are the "same" operand. err on
   the side of caution; false negatives are OK, false positives not. 

   for now only recognizes same registers. */

static 
peep_same(t1, t2)
    struct tree * t1;
    struct tree * t2;
{
    if (t1 == NULL) return 0;
    if (t2 == NULL) return 0;
    if (t1->op != t2->op) return 0;
    if (t1->type->ts != t2->type->ts) return 0;

    if (t1->op == E_REG) return (t1->u.reg == t2->u.reg);

    return 0;
}

/* return non-zero if the sequence starting at 'insn' in 'block'
   matches the template 'pm', zero otherwise. */

peep_match(block, insn, pm)
    struct block      * block;
    struct insn       * insn;
    struct peep_match * pm;
{
    int             i;
    int             j;
    int             ts;
    int             pmos;
    int             pmis;
    struct tree   * operand;
    struct defuse * defuse;
    
    for (i = 0; pm[i].opcode != I_NONE; ++i, insn = insn->next) {
        pmis = pm[i].pmis;
        pm[i].tmp = insn;
        if (insn == NULL) return 0; 
        if ((pm[i].opcode != I_ANY) && (pm[i].opcode != insn->opcode)) return 0;
        if ((pmis & PMI_CCS_UNUSED) && (insn->flags & INSN_FLAG_CC)) return 0;

        for (j = 0; ts = pm[i].operand[j].ts; ++j) {
            pmos = pm[i].operand[j].pmos;
            operand = insn->operand[j];
            if (operand == NULL) return 0; 
            if (!(ts & operand->type->ts)) return 0;

            if (pmos & PMO_REG) {
                if (operand->op != E_REG) return 0;
                if ((pmos & PMO_DEAD) && !reg_is_dead(block, insn, operand->u.reg)) return 0;
                if ((pmos & PMO_DEF) && !insn_defs_reg(insn, operand->u.reg)) return 0;
                if ((pmos & PMO_USE) && !insn_uses_reg(insn, operand->u.reg)) return 0;
                if ((pmos & PMO_NOTDEF) && insn_defs_reg(insn, operand->u.reg)) return 0;
                if ((pmos & PMO_NOTUSE) && insn_uses_reg(insn, operand->u.reg)) return 0;

                if (pmos & PMO_UNALIASED) {
                    defuse = find_defuse(block, operand->u.reg, FIND_DEFUSE_NORMAL);
                    if (!defuse) return 0;
                    if (!(defuse->symbol->ss & S_REGISTER)) return 0;
                }
            }

            if (pmos & PMO_CON) {
                if (operand->op != E_CON) return 0;
                if ((pmos & PMO_VALUE) && (operand->u.con.i != pm[i].operand[j].value)) return 0;
            }

            if (pmos & PMO_SAME) {
                int other_insn = PMO_SAME_GET_INSN(pmos);
                int other_operand = PMO_SAME_GET_OPERAND(pmos);
                if (!peep_same(operand, pm[other_insn].tmp->operand[other_operand])) return 0;
            }
        }
    }

    return 1;
}

