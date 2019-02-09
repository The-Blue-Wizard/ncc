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

#include <stdlib.h>
#include <stdarg.h>
#include "ncc1.h"

/* place a block on the master list before another block.
   if 'before' is NULL, the block goes at the end. */

static void
put_block(struct block * block, struct block * before)
{
    block->next = before;

    if (block->next == NULL) {
        block->previous = last_block;
        last_block = block;
    } else {
        block->previous = block->next->previous;
        block->next->previous = block;
    }

    if (block->previous == NULL)
        first_block = block;
    else
        block->previous->next = block;
}

/* remove a block from the master list. */

static void
get_block(struct block * block)
{
    if (block->next)
        block->next->previous = block->previous;
    else
        last_block = block->previous;

    if (block->previous)
        block->previous->next = block->next;
    else
        first_block = block->next;
}

/* create a new, empty block, and place it on the end of the master list. */

struct block *
new_block(void)
{
    struct block * block;
    int            i;

    block = (struct block *) allocate(sizeof(struct block));
    block->asm_label = next_asm_label++;
    block->bs = 0;
    block->nr_insns = 0;
    block->loop_level = loop_level;
    block->first_insn = NULL;
    block->last_insn = NULL;
    block->successors = NULL;
    block->predecessors = NULL;
    block->nr_successors = 0;
    block->nr_predecessors = 0;
    block->defuses = NULL;

    for (i = 0; i < NR_REGS; i++) {
        block->iregs[i] = NULL;
        block->fregs[i] = NULL;
    }

    put_block(block, NULL);     /* will set ->next and ->previous */
    return block;
}

/* indicate that one block succeeds another under the given condition.
   we allow a caller to invoke this with CC_NEVER, but take no action.
   this eliminates special cases in the parser. */

void
succeed_block(struct block * block, int cc, struct block * successor)
{
    struct block_list * list;

    if (cc != CC_NEVER) {
        list = (struct block_list *) allocate(sizeof(struct block_list));
        list->cc = cc;
        list->block = successor;
        list->link = block->successors;
        block->successors = list;
        block->nr_successors++;

        list = (struct block_list *) allocate(sizeof(struct block_list));
        list->cc = CC_NONE;
        list->block = block;
        list->link = successor->predecessors;
        successor->predecessors = list;
        successor->nr_predecessors++;
    }
}

/* undo the work of succeed_block() */

void
unsucceed_block(struct block * block, int n)
{
    struct block       * successor;
    struct block_list ** listp;
    struct block_list  * tmp;

    listp = &(block->successors);
    while (n--) listp = &((*listp)->link);
    successor = (*listp)->block;
    tmp = *listp;
    *listp = tmp->link;
    free(tmp);
    block->nr_successors--;

    listp = &(successor->predecessors);
    while ((*listp)->block != block) listp = &((*listp)->link);
    tmp = *listp;
    *listp = tmp->link;
    free(tmp);
    successor->nr_predecessors--;
}

/* return the 'n'th successor or predecessor of 'block', or NULL. */

static struct block *
cessor1(struct block_list * list, int n)
{
    while (n-- && list) list = list->link;

    if (list)
        return list->block;
    else
        return NULL;
}

struct block *
block_successor(struct block * block, int n)
{
    return cessor1(block->successors, n);
}

struct block *
block_predecessor(struct block * block, int n)
{
    return cessor1(block->predecessors, n);
}

/* return the condition require to succeed from block to successor n. */

int
block_successor_cc(struct block * block, int n)
{
    struct block_list * list;

    list = block->successors;
    while (n--) list = list->link;
    return list->cc;
}

/* called before the first statement of a function definition is parsed
   to set up the initial block structure (entry, exit, and current). */

void
setup_blocks(void)
{
    entry_block = new_block();
    exit_block = new_block();
    current_block = new_block();
    succeed_block(entry_block, CC_ALWAYS, current_block);
}

/* free all the def/use information associated with a block. */

static void
free_defuses(struct block * block)
{
    struct defuse * defuse;
    struct defuse * tmp;

    for (defuse = block->defuses; defuse; defuse = tmp) {
        tmp = defuse->link;
        free(defuse);
    }

    block->defuses = NULL;
}

/* free an instruction and its operands */

static void
free_insn(struct insn * insn)
{
    int i;

    for (i = 0; i < I_NR_OPERANDS(insn->opcode); i++)
        free_tree(insn->operand[i]);

    free(insn);
}

/* remove an instruction from its block */

static void
get_insn(struct block * block, struct insn * insn)
{
    if (insn->next)
        insn->next->previous = insn->previous;
    else
        block->last_insn = insn->previous;

    if (insn->previous)
        insn->previous->next = insn->next;
    else
        block->first_insn = insn->next;

    block->nr_insns--;
}

/* free a block and all its resources. */

void
free_block(struct block * block)
{
    struct block_list * list;
    struct insn *       insn;

    get_block(block);

    while (insn = block->first_insn) {
        get_insn(block, insn);
        free_insn(insn);
    }

    while (list = block->successors) {
        block->successors = list->link;
        free(list);
    }

    while (list = block->predecessors) {
        block->predecessors = list->link;
        free(list);
    }

    free_defuses(block);
    free(block);
}

/* called after code generation is complete */

void
free_blocks(void)
{
    while (first_block) free_block(first_block);

    entry_block = NULL;
    exit_block = NULL;
    current_block = NULL;
}

/* create a new instruction with up to three operands. ownership
   of the operand trees is yielded by the caller. */

struct insn *
new_insn(int opcode, ...)
{
    struct insn * insn;
    va_list       args;

    va_start(args, opcode);
    insn = (struct insn *) allocate(sizeof(struct insn));
    insn->opcode = opcode;
    insn->operand[0] = (I_NR_OPERANDS(opcode) >= 1) ? va_arg(args, struct tree *) : NULL;
    insn->operand[1] = (I_NR_OPERANDS(opcode) >= 2) ? va_arg(args, struct tree *) : NULL;
    insn->operand[2] = (I_NR_OPERANDS(opcode) >= 3) ? va_arg(args, struct tree *) : NULL;
    insn->next = NULL;
    insn->previous = NULL;
    insn->flags = 0;
    va_end(args);
    return insn;
}


/* put an instruction into the instruction list in a block, 
   at the specified position. if 'before' is NULL, the put it last. */

void
put_insn(struct block * block, struct insn * insn, struct insn * before)
{
    insn->next = before;

    if (insn->next == NULL) {
        insn->previous = block->last_insn;
        block->last_insn = insn;
    } else {
        insn->previous = insn->next->previous;
        insn->next->previous = insn;
    }

    if (insn->previous == NULL) 
        block->first_insn = insn;
    else
        insn->previous->next = insn;

    block->nr_insns++;
}

/* remove an instruction from its block and free it */

void
kill_insn(struct block * block, struct insn * insn)
{
    get_insn(block, insn);
    free_insn(insn);    
}

/* fill in the regs_defd and regs_used fields of an instruction */

static void
analyze_insn1(int * regs, int reg)
{
    int i;

    for (i = 0; i < NR_INSN_REGS; i++) {
        if (regs[i] == reg) break;

        if (regs[i] == R_NONE) {
            regs[i] = reg;
            break;
        }
    }
    
    if (i == NR_INSN_REGS) error(ERROR_INTERNAL); /* overflow */
}

static void
analyze_insn(struct insn * insn)
{
    int i;

    for (i = 0; i < NR_INSN_REGS; i++) {
        insn->regs_used[i] = R_NONE;
        insn->regs_defd[i] = R_NONE;
    }

    insn->mem_defd = 0;
    insn->mem_used = 0;

    if (insn->opcode == I_CALL) {
        insn->mem_defd = 1;
        insn->mem_used = 1;
    } else {
        insn->mem_defd = 0;
        insn->mem_used = 0;
    }

    if (insn->opcode & I_DEF_AX) analyze_insn1(insn->regs_defd, R_AX);
    if (insn->opcode & I_USE_AX) analyze_insn1(insn->regs_used, R_AX);
    if (insn->opcode & I_DEF_DX) analyze_insn1(insn->regs_defd, R_DX);
    if (insn->opcode & I_USE_DX) analyze_insn1(insn->regs_used, R_DX);
    if (insn->opcode & I_DEF_CX) analyze_insn1(insn->regs_defd, R_CX);
    if (insn->opcode & I_DEF_XMM0) analyze_insn1(insn->regs_defd, R_XMM0);

    for (i = 0; i < I_NR_OPERANDS(insn->opcode); i++) {
        if (insn->operand[i]->op == E_REG) {
            if (insn->opcode & I_DEF(i)) 
                analyze_insn1(insn->regs_defd, insn->operand[i]->u.reg);
            if (insn->opcode & I_USE(i)) 
                analyze_insn1(insn->regs_used, insn->operand[i]->u.reg);
        } else if (insn->operand[i]->op == E_MEM) {
            if (insn->opcode & I_DEF(i)) insn->mem_defd++;
            if (insn->opcode & I_USE(i)) insn->mem_used++;

            if (insn->operand[i]->u.mi.b != R_NONE) 
                analyze_insn1(insn->regs_used, insn->operand[i]->u.mi.b);
            if (insn->operand[i]->u.mi.i != R_NONE) 
                analyze_insn1(insn->regs_used, insn->operand[i]->u.mi.i);
        }
    }
}

/* replace references to reg 'x' with 'y' in an instruction.
   this invalidates regs_used[] and regs_defd[]. */

void
insn_replace_reg(struct insn * insn, int x, int y)
{
    int i;

    for (i = 0; i < I_NR_OPERANDS(insn->opcode); i++) {
        if (insn->operand[i]->op == E_REG) {
            if (insn->operand[i]->u.reg == x) insn->operand[i]->u.reg = y;
        } else if (insn->operand[i]->op == E_MEM) {
            if (insn->operand[i]->u.mi.b == x) insn->operand[i]->u.mi.b = y;
            if (insn->operand[i]->u.mi.i == x) insn->operand[i]->u.mi.i = y;
        }
    }
}

/* returns non-zero if the given register can be 
   replaced with another in the given instruction.
   the answer is 'yes' unless the register is 
   implicit in the instruction. */

int
insn_reg_replaceable(struct insn * insn, int reg)
{
    int mask = 0;

    switch (reg)
    {
    case R_AX: mask = I_DEF_AX | I_USE_AX; break;
    case R_DX: mask = I_DEF_DX | I_USE_DX; break;
    case R_CX: mask = I_DEF_CX; break;
    case R_XMM0: mask = I_DEF_XMM0; break;
    }

    if (insn->opcode & mask) 
        return 0;
    else
        return 1;
}

/* does 'reg' appear in 'regs[]'? */

static int
insn_reg1(int * regs, int reg) 
{
    int i;

    for (i = 0; i < NR_INSN_REGS; i++) {
        if (regs[i] == reg) return 1;
        if (regs[i] == R_NONE) break;
    }
    
    return 0;
}

/* is 'reg' DEFd in 'insn'? */

int
insn_defs_reg(struct insn * insn, int reg)
{
    return insn_reg1(insn->regs_defd, reg);
}

/* is 'reg' USEd in 'insn'? */

int
insn_uses_reg(struct insn * insn, int reg)
{
    return insn_reg1(insn->regs_used, reg);
}

/* does 'reg' appear in 'insn'? */

int
insn_touches_reg(struct insn * insn, int reg)
{
    return (insn_uses_reg(insn, reg) || insn_defs_reg(insn, reg));
}

/* how many members of 'regs[]'? */

static int
insn_reg_count(int * regs)  
{
    int i;

    for (i = 0; i < NR_INSN_REGS; ++i) 
        if (regs[i] == R_NONE) 
            break;

    return i;
}

/* how many regs are DEFd in 'insn'? */

int
insn_nr_defs(struct insn * insn)
{
    return insn_reg_count(insn->regs_defd);
}

/* how many regs are USEd in 'insn'? */

int
insn_nr_uses(struct insn * insn)
{
    return insn_reg_count(insn->regs_used);
}

/* split a block in half: create a new block, move the latter half of 
   the insns to the new block, and play with successors to maintain the
   original flow. used by the register allocator. */

void
split_block(struct block * block)
{
    struct block * latter;
    struct insn  * insn;
    int            cc;
    struct block * successor;

    latter = new_block();
    latter->loop_level = block->loop_level;

    while (latter->nr_insns < block->nr_insns) {     /* move roughly half */
        insn = block->last_insn;
        get_insn(block, insn);
        put_insn(latter, insn, latter->first_insn);
    }

    while (successor = block_successor(block, 0)) {
        cc = block_successor_cc(block, 0);
        unsucceed_block(block, 0);
        succeed_block(latter, cc, successor);
    }

    succeed_block(block, CC_ALWAYS, latter);
}

/* order and analyze all the instructions, and determine
   some basic register-allocation information for the block. */

static void
analyze_block(struct block * block)
{
    struct insn * insn;
    struct insn * last_cc = NULL;
    int           n;

    block->prohibit_iregs = (1 << R_IDX(R_BP)) | (1 << R_IDX(R_SP));
    block->temponly_iregs = (   (1 << R_IDX(R_AX)) 
                            |   (1 << R_IDX(R_CX)) 
                            |   (1 << R_IDX(R_DX)) );

    block->prohibit_fregs = 0;
    block->temponly_fregs = 1 << R_IDX(R_XMM0);

    for (insn = block->first_insn, n = 1; insn; insn = insn->next, ++n) {
        insn->n = n;
        analyze_insn(insn);

        if (insn_touches_reg(insn, R_AX)) 
            block->prohibit_iregs |= 1 << R_IDX(R_AX);
        if (insn_touches_reg(insn, R_DX)) 
            block->prohibit_iregs |= 1 << R_IDX(R_DX);
        if (insn_touches_reg(insn, R_CX)) 
            block->prohibit_iregs |= 1 << R_IDX(R_CX);
        if (insn_touches_reg(insn, R_XMM0)) 
            block->prohibit_fregs |= 1 << R_IDX(R_XMM0);

        if ((insn->opcode & I_USE_CC) && last_cc) 
            last_cc->flags |= INSN_FLAG_CC;

        if (insn->opcode & I_DEF_CC) last_cc = insn;
    }

    /* the last condition codes set are ignored if 
       the block only has an unconditional successor */

    if (last_cc && (block->nr_successors > 1)) last_cc->flags |= INSN_FLAG_CC;  
}

/* analyze all blocks. then, if a register isn't prohibited in any
   block, then it isn't subject to a temponly restriction. this mainly
   opens up AX, CX, DX and XMM0 in leaf routines to global allocation. */

static void
analyze_blocks(void)
{
    struct block * block;
    int            prohibit_iregs = 0;
    int            prohibit_fregs = 0;

    for (block = first_block; block; block = block->next) {
        analyze_block(block);
        prohibit_iregs |= block->prohibit_iregs;
        prohibit_fregs |= block->prohibit_fregs;
    }

    for (block = first_block; block; block = block->next) {
        block->temponly_iregs &= prohibit_iregs;
        block->temponly_fregs &= prohibit_fregs;
    }
}

/* put the master block list in depth-first order */

static void
sequence1(struct block * block)
{
    struct block * successor;
    int            n;

    if (!(block->bs & B_SEQ)) {
        block->bs |= B_SEQ;

        for (n = 0; successor = block_successor(block, n); ++n)
            sequence1(successor);

        get_block(block);
        put_block(block, first_block);
    }
}

void
sequence_blocks(void)
{
    struct block * block;

    for (block = first_block; block; block = block->next)
        block->bs &= ~B_SEQ;

    sequence1(first_block);
}

/* find the def/use information associated with a (pseudo) register in the 
   specified block. if mode is FIND_DEFUSE_CREATE, this is guaranteed to 
   return an entry, otherwise NULL is returned if no def/use data exists. */

struct defuse *
find_defuse(struct block * block, int reg, int mode)
{
    struct defuse * defuse;
    struct symbol * symbol;

    for (defuse = block->defuses; defuse; defuse = defuse->link) 
        if (defuse->symbol->reg == reg) return defuse;

    if (mode != FIND_DEFUSE_CREATE) return NULL;

    symbol = find_symbol_by_reg(reg);
    if (symbol == NULL) error(ERROR_INTERNAL);

    defuse = (struct defuse *) allocate(sizeof(struct defuse));
    defuse->symbol = symbol;
    defuse->dus = 0;
    defuse->reg = R_NONE;
    defuse->first_n = 0;
    defuse->last_n = 0;
    defuse->distance = 0;
    defuse->con = 0;
    defuse->cache = DU_CACHE_INVALID;
    defuse->link = block->defuses;
    block->defuses = defuse;
    return defuse;
}

/* like above, except no ability to create non-existent entries. */

struct defuse *
find_defuse_by_symbol(struct block * block, struct symbol * symbol)
{
    struct defuse * defuse;

    for (defuse = block->defuses; defuse; defuse = defuse->link) 
        if (defuse->symbol == symbol) return defuse;

    return NULL;
}

/* (re)compute the local def/use data for the block. */

static void
compute_block_defuses1(struct block * block, struct insn * insn, int du)
{
    struct defuse * defuse;
    int           * regs;
    int             reg;
    int             i;

    regs = (du == DU_USE) ? insn->regs_used : insn->regs_defd;

    for (i = 0; i < NR_INSN_REGS; i++) {
        reg = regs[i];
        if (reg == R_NONE) break;
        if (!R_IS_PSEUDO(reg)) continue;
        defuse = find_defuse(block, reg, FIND_DEFUSE_CREATE);

        if (!(defuse->dus & (DU_USE | DU_DEF))) {
            defuse->first_n = insn->n;
            defuse->dus |= du;
        }

        defuse->last_n = insn->n;
    }
}

static void
compute_block_defuses(struct block * block)
{
    struct insn   * insn;

    free_defuses(block);

    for (insn = block->first_insn; insn; insn = insn->next) {
        compute_block_defuses1(block, insn, DU_USE);
        compute_block_defuses1(block, insn, DU_DEF);
    }
}

/* compute global def/use data. this destroys any existing def/use data. */

void
compute_global_defuses(void)
{
    struct block  * block;
    struct block  * successor;
    struct defuse * defuse;
    struct defuse * succ_defuse;
    int             changes;
    int             n;
    int             distance;

    analyze_blocks();

    for (block = first_block; block; block = block->next) 
        compute_block_defuses(block);

    do {
        changes = 0;

        for (block = first_block; block; block = block->next) {
            for (n = 0; successor = block_successor(block, n); ++n) {
                for (succ_defuse = successor->defuses; succ_defuse; succ_defuse = succ_defuse->link) {
                    if (succ_defuse->dus & DU_IN) {
                        defuse = find_defuse(block, succ_defuse->symbol->reg, FIND_DEFUSE_CREATE);
                        distance = DU_TRANSIT(*defuse) ? (succ_defuse->distance + 1) : 0;

                        if (!(defuse->dus & DU_OUT)) {
                            changes++;
                            defuse->dus |= DU_OUT;
                            defuse->distance = distance;
                        } 
                        
                        if (defuse->distance > distance) {
                            defuse->distance = distance;
                            changes++;
                        }
                    }
                }
            }

            for (defuse = block->defuses; defuse; defuse = defuse->link) {
                if (((defuse->dus & DU_OUT) && !(defuse->dus & DU_DEF)) || (defuse->dus & DU_USE)) {
                    if (!(defuse->dus & DU_IN)) {
                        defuse->dus |= DU_IN;
                        changes++;
                    }
                }
            }
        }
    } while (changes);
}

/* is 'reg' dead after 'insn' in 'block'?
   a [pseudo] register is considered dead if:
   1. it's S_REGISTER, 
   2. its last appearance in 'block' is at or before 'insn',
   3. it's not live out of the block. */

int
reg_is_dead(struct block * block, struct insn * insn, int reg)
{
    struct defuse * defuse;

    defuse = find_defuse(block, reg, FIND_DEFUSE_NORMAL);
    if (defuse == NULL) return 0;
    if (!(defuse->symbol->ss & S_REGISTER)) return 0;
    if (defuse->last_n > insn->n) return 0;
    if (defuse->dus & DU_OUT) return 0;

    return 1;
}
