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

/* simple jump optimization -- replace jumps to empty blocks with
   unconditional successors with direct jumps to those successors.
   this is really a requirement for half-decent code, given the
   abandon with which the parser generates empty blocks. as such,
   it's run whether -O is given or not. */

static void
jumps(void)
{
    struct block * block;
    struct block * successor;
    struct block * successor_successor;
    int            cc;
    int            n;
    int            changes;

    do {
        changes = 0;
        for (block = first_block; block; block = block->next) {
            for (n = 0; successor = block_successor(block, n); ++n) {
                if ((successor->nr_insns == 0) && (successor->nr_successors == 1)) {
                    successor_successor = block_successor(successor, 0);
                    cc = block_successor_cc(successor, 0);

                    if ((cc == CC_ALWAYS) && (successor != successor_successor)) {
                        cc = block_successor_cc(block, n);
                        unsucceed_block(block, n);
                        succeed_block(block, cc, successor_successor);
                        ++changes;
                    }
                }
            }
        }
    } while (changes);
}

/* remove unreachable code, i.e., blocks with no predecessors. 
   we never remove the entry or exit blocks. like jumps(), this
   is run whether -O is given or not, and for the same reason. */

static void
unreachable(void)
{
    struct block * block;
    struct block * successor;

    again:
    for (block = first_block; block; block = block->next) {
        if (block == entry_block) continue;
        if (block == exit_block) continue;

        if (block->nr_predecessors == 0) {
            while (successor = block_successor(block, 0))
                unsucceed_block(block, 0);

            free_block(block);
            goto again; 
        }
    }
}


/* convert S_LOCALs to S_REGISTER. error on any undefined labels. */

static void
walk1(struct symbol * symbol)
{
    if (symbol->ss & S_LOCAL) {
        symbol->ss &= ~S_LOCAL;
        symbol->ss |= S_REGISTER;
    }

    if ((symbol->ss & S_LABEL) && !(symbol->ss & S_DEFINED))
        error(ERROR_DANGLING);
}

/* generate function prologue and epilogue in entry and exit blocks */

static void
logues(void)
{
    struct tree   * reg;
    int             i;
    long            locals;
    struct tree   * floats[NR_REGS];
    struct symbol * tmp;

    for (i = 0; i < NR_REGS; i++) {
        if (save_fregs & (1 << i)) {
            tmp = temporary_symbol(new_type(T_DOUBLE));
            floats[i] = memory_tree(tmp);
        } else
            floats[i] = NULL;
    }

    frame_offset = ROUND_UP(frame_offset, FRAME_ALIGN);
    locals = frame_offset;

    put_insn(entry_block, new_insn(I_PUSH, reg_tree(R_BP, new_type(T_LONG))), NULL);
    put_insn(entry_block, new_insn(I_MOV, reg_tree(R_BP, new_type(T_LONG)), reg_tree(R_SP, new_type(T_LONG))), NULL);
    if (locals) put_insn(entry_block, new_insn(I_SUB, reg_tree(R_SP, new_type(T_LONG)), int_tree(T_LONG, locals)), NULL);
    
    for (i = 0; i < NR_REGS; i++) {
        if (save_iregs & (1 << i)) {
            reg = reg_tree(R_AX + i, new_type(T_LONG));
            put_insn(entry_block, new_insn(I_PUSH, copy_tree(reg)), NULL);
            put_insn(exit_block, new_insn(I_POP, reg), exit_block->first_insn);
        }

        if (save_fregs & (1 << i)) {
            reg = reg_tree(R_XMM0 + i, new_type(T_DOUBLE));
            put_insn(entry_block, new_insn(I_MOVSD, copy_tree(floats[i]), copy_tree(reg)), NULL);
            put_insn(exit_block, new_insn(I_MOVSD, reg, floats[i]), exit_block->first_insn);
        }
    }

    if (locals) put_insn(exit_block, new_insn(I_ADD, reg_tree(R_SP, new_type(T_LONG)), int_tree(T_LONG, locals)), NULL);
    put_insn(exit_block, new_insn(I_POP, reg_tree(R_BP, new_type(T_LONG))), NULL);
    put_insn(exit_block, new_insn(I_RET), NULL);
}

/* peephole optimizations */

static int
peeps(struct block * block)
{
    struct insn * insn;

    for (insn = block->first_insn; insn; insn = insn->next) 
    {
        /* AND <reg>, <??> -> TEST <reg>, <??> */

        {
            static struct peep_match test[] = 
            {
                { I_AND, 0, { { T_IS_SCALAR, PMO_REG | PMO_DEAD } } },
                { I_NONE }
            };

            if (peep_match(block, insn, test)) 
            {
                insn->opcode = I_TEST;
                return -1;
            }
        }
    }

    return 0;
}


/* optimizations that are simple one-for-one substitutions.
   these aren't processed until the last minute because they 
   have the potential to obscure other optimizations. */

static void
subs(struct block * block)
{
    struct insn * insn;
    int           i;

    for (insn = block->first_insn; insn; insn = insn->next) {
        { 
            /* ADD <??>, 1 -> INC <??>
               SUB <??>, -1 -> INC <??> */

            static struct peep_match test1[] = 
            {
                { I_ADD, PMI_CCS_UNUSED, { { T_IS_SCALAR }, { T_IS_SCALAR, PMO_CON | PMO_VALUE, 1 } } },
                { I_NONE }
            };

            static struct peep_match test2[] = 
            {
                { I_SUB, PMI_CCS_UNUSED, { { T_IS_SCALAR }, { T_IS_SCALAR, PMO_CON | PMO_VALUE, -1 } } },
                { I_NONE }
            };

            if (peep_match(block, insn, test1) || peep_match(block, insn, test2)) {
                insn->opcode = I_INC;
                free_tree(insn->operand[1]);
                insn->operand[1] = NULL;
            }
        }

        { 
            /* ADD <??>, -1 -> DEC <??>
               SUB <??>, 1 -> DEC <??> */

            static struct peep_match test1[] = 
            {
                { I_SUB, PMI_CCS_UNUSED, { { T_IS_SCALAR }, { T_IS_SCALAR, PMO_CON | PMO_VALUE, 1 } } },
                { I_NONE }
            };

            static struct peep_match test2[] = 
            {
                { I_ADD, PMI_CCS_UNUSED, { { T_IS_SCALAR }, { T_IS_SCALAR, PMO_CON | PMO_VALUE, -1 } } },
                { I_NONE }
            };

            if (peep_match(block, insn, test1) || peep_match(block, insn, test2)) {
                insn->opcode = I_DEC;
                free_tree(insn->operand[1]);
                insn->operand[1] = NULL;
            }
        }

        /* easy ones left on the table:
            IMUL by a constant power of two = SHL
            MOV <reg>, 0 = XOR <reg>, <reg> */
    }
}

/* reminder to do a limited local form of register coalescing 
   to, among other things, clean up the code generator temps */

static int
coalesce(struct block * block)
{  
    return 0;
}

/* remove dead code (dead stores): any instruction that
   only DEFs a register whose value is never used, and
   has no other side effects, is dead code. */

static int
dead_stores(struct block * block)
{
    struct insn * insn;
    struct insn * next;
    int           kills = 0;

    for (insn = block->first_insn; insn; insn = next) {
        next = insn->next;

        /* no side effects:
           1. can't set condition codes that are inspected
           2. no memory reads
           3. no memory writes
           4. only one register DEFd
           5. reg is dead after insn */

        if (insn->flags & INSN_FLAG_CC) continue; 
        if (insn->mem_used) continue;
        if (insn->mem_defd) continue;      
        if (insn_nr_defs(insn) != 1) continue; 
        if (!reg_is_dead(block, insn, insn->regs_defd[0])) continue;

        kill_insn(block, insn);
        ++kills;
    }

    return -kills; 
}

/* constant propogation. step through the block and track if a register
   has a known constant value; use DU_CON and 'con' to track. we should  
   use this information to replace the register with the constant value 
   in subsequent insns, but we don't yet. for now, the data is only used
   to eliminate jumps. see end of loop. */ 

static int
con_prop(struct block * block)
{
    struct insn    * insn;
    int             i;
    int             j;
    int             reg;
    struct defuse * defuse;
    struct block  * test_block;
    struct block  * succ;

    static struct peep_match mov_con[] = 
    {
        { I_MOV, 0, { { T_INT, PMO_REG | PMO_UNALIASED }, { T_INT, PMO_CON } } },
        { I_NONE }
    };

    static struct peep_match test[] = 
    {
        { I_TEST, 0, { { T_IS_INT, PMO_REG }, { T_IS_INT, PMO_REG | PMO_SAME_AS(0, 0) } } },
        { I_NONE }
    };
    
    for (insn = block->first_insn; insn; insn = insn->next) {
        /* first, invalidate all the
           registers DEFd in this insn */

        j = insn_nr_defs(insn);

        for (i = 0; i < j; ++i) {
            defuse = find_defuse(block, insn->regs_defd[i], FIND_DEFUSE_NORMAL);
            if (defuse) defuse->dus &= ~DU_CON;
        }

        if (peep_match(block, insn, mov_con)) {
            /* a MOV <reg>, <con> .. remember its value */

            reg = insn->operand[0]->u.reg;
            defuse = find_defuse(block, reg, FIND_DEFUSE_NORMAL);

            if (defuse) {
                defuse->dus |= DU_CON;
                defuse->con = insn->operand[1]->u.con.i;
            }
        } else {
            /* propagate constants here! */
        }
    }

    /* look to see if we have exactly one successor whose 
       sole purpose is to TEST a value whose value we know
       is constant; we can jump around the test. this, in 
       concert with dead code and jump optimization, makes
       the output for logical operators much, much better.
       the code generator just doesn't have enough context. */

    if (block->nr_successors != 1) return 0;
    test_block = block_successor(block, 0);
    if (test_block->nr_insns != 1) return 0;
    insn = test_block->first_insn;
    if (!peep_match(test_block, insn, test)) return 0;
    reg = insn->operand[0]->u.reg;
    defuse = find_defuse(block, reg, FIND_DEFUSE_NORMAL);
    if (!defuse) return 0;
    if (!(defuse->dus & DU_CON)) return 0;

    for (i = 0; succ = block_successor(test_block, i); ++i) {
        j = block_successor_cc(test_block, i);
        if ((defuse->con == 0) && (j == CC_Z)) break;
        if ((defuse->con != 0) && (j == CC_NZ)) break;
    }
  
    if (succ) {
        unsucceed_block(block, 0);
        succeed_block(block, CC_ALWAYS, succ);
        return -1;
    } else
        return 0;
}

/* copy propagation: when an unaliased copy of another unaliased 
   register is made, replace subsequent appearances of the latter 
   with the former, until either register is modified. */

static int
copy_prop(struct block * block)
{
    static struct peep_match copy[] = 
    {
        { I_MOV, 0, { { T_IS_SCALAR, PMO_REG | PMO_UNALIASED }, { T_IS_SCALAR, PMO_REG | PMO_UNALIASED } } },
        { I_NONE }
    };

    struct defuse * defuse;
    struct insn   * insn;
    int             i;
    int             reg;
    int             ret = 0;

    for (insn = block->first_insn; insn; insn = insn->next) {
        /* invalidate copy information for any registers DEFd */
        
        for (i = insn_nr_defs(insn) - 1; i >= 0; --i) {
            reg = insn->regs_defd[i];

            for (defuse = block->defuses; defuse; defuse = defuse->link) {
                if (    (defuse->reg == reg)
                    ||  ((defuse->dus & DU_COPY) && (defuse->copy == reg)) )
                {
                    defuse->dus &= ~DU_COPY;
                }
            }
        }

        /* remember copies */

        if (peep_match(block, insn, copy)) {
            reg = insn->operand[0]->u.reg;
            defuse = find_defuse(block, reg, FIND_DEFUSE_NORMAL);

            if (defuse) {
                defuse->dus |= DU_COPY;
                defuse->copy = insn->operand[1]->u.reg;
            }

            continue;
        } 

        /* replace copies */

        for (i = insn_nr_uses(insn) - 1; i >= 0; --i) {
            reg = insn->regs_used[i];
            
            if (    !insn_defs_reg(insn, reg)
                &&  insn_reg_replaceable(insn, reg)
                &&  (defuse = find_defuse(block, reg, FIND_DEFUSE_NORMAL))
                &&  (defuse->dus & DU_COPY) )
            {
                insn_replace_reg(insn, reg, defuse->copy);
                ret = -1;
            }
        }
    }

    return ret;
}

/* optimizer functions. these are executed in the order in which
   they appear here, and that order is potentially important */

struct optimizer
{
    int     level;
    int ( * func ) (struct block *);
} optimizers[] = {
    { 1, peeps },  
    { 1, coalesce },
    { 1, dead_stores },     
    { 1, con_prop },
    { 1, copy_prop }
};

#define NR_OPTIMIZERS (sizeof(optimizers)/sizeof(*optimizers))

/* optimize() is a bit of a misnomer. it doesn't just handle 
   optimization - it drives the whole code generation process. */

void
optimize(void)
{   
    struct block * block;
    int            again;
    int            ret;
    int            i;

    succeed_block(current_block, CC_ALWAYS, exit_block);
    walk_symbols(SCOPE_FUNCTION, SCOPE_RETIRED, walk1);

    /* optimize in a loop until no more optimizations are done.
       each local optimization function will return non-zero if
       it made any changes - negative if data flow is invalidated.

       for now, we're extremely conservative about maintaining
       data flow information - if an optimizer reports that the 
       data has been invalidated for the block, we recompute ALL
       the data (globally) and restart from the beginning. this 
       obviously has very poor asymptotic properties, so the
       optimizers should try to incrementally update the data. */
    
    do  
    {
      restart:
        again = 0;
        jumps(); 
        unreachable();
        compute_global_defuses();

        for (block = first_block; block; block = block->next) {
            for (i = 0; i < NR_OPTIMIZERS; ++i) {
                if (optimizers[i].level <= O_flag) {
                    ret = optimizers[i].func(block);
                    again += ret;
                    if (ret < 0) goto restart;
                }
            }
        }
    } while (again);

    if (O_flag) {
        for (block = first_block; block; block = block->next)
            subs(block);
    }

    allocate_regs();
    frame_offset = ROUND_UP(frame_offset, FRAME_ALIGN);
    logues();
}
