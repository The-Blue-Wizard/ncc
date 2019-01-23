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

/* convert a floating-point E_CON into an E_MEM node referencing
   an anonymous, static symbol in the text segment. */

struct tree *
float_literal(struct tree * tree)
{
    struct symbol * symbol;

    symbol = new_symbol(NULL, S_STATIC, copy_type(tree->type));
    symbol->i = next_asm_label++;
    put_symbol(symbol, SCOPE_RETIRED);
    segment(SEGMENT_TEXT);
    output("%G: %s %O\n", symbol, (tree->type->ts & T_DOUBLE) ? ".qword" : ".dword", tree);
    free_tree(tree);
    return memory_tree(symbol);
}

/* this function tracks state to achieve bit granularity in initializer output. */

static void
initialize_bits(long i, int n)
{
    static char buf;
    static char pos;

    while (n--) {
        buf >>= 1;

        if (i & 1) 
            buf |= 0x80;
        else
            buf &= 0x7F;

        i >>= 1;
        pos++;
        if ((pos % 8) == 0) output(" .byte %d\n", buf & 255);
    }
}

static void initialize(struct type *, int);

/* read one scalar value and output it to the current 
   position in the data segment. 'type' is NOT consumed. */

static void
initialize_scalar(struct type * type, int outermost)
{
    struct symbol * symbol;
    struct tree *   tree;
    int             braced = 0;

    if (outermost && (token.kk == KK_LBRACE)) {
        ++braced;
        lex();
    }

    tree = fake_assignment(type);
    tree = generate(tree, GOAL_VALUE, NULL);
    if ((tree->op != E_CON) && (tree->op != E_IMM)) error(ERROR_BADINIT);

    /* static/extern address have RIP set - lose it */

    if (tree->op == E_IMM) {
        tree->u.mi.rip = 0;
        if ((tree->u.mi.b != R_NONE) || (tree->u.mi.i != R_NONE)) error(ERROR_BADINIT);
    }

    if (type->ts & T_FIELD) {
        if (tree->op != E_CON) error(ERROR_BADINIT);
        initialize_bits(tree->u.con.i, T_GET_SIZE(type->ts));
    } else {
        if (type->ts & T_IS_BYTE) output(" .byte ");
        if (type->ts & T_IS_WORD) output(" .word ");
        if (type->ts & T_IS_DWORD) output(" .dword ");
        if (type->ts & T_IS_QWORD) output(" .qword ");
        output("%O\n", tree);
    }

    free_tree(tree);
    if (braced) match(KK_RBRACE);
}

static void
initialize_array(struct type * type, int outermost)
{
    struct type * element_type;
    int           nr_elements = 0;
    int           braced = 0;

    element_type = type->next;

    if (token.kk == KK_LBRACE) {
        ++braced;
        lex();
    }

    if ((element_type->ts & T_IS_CHAR) && (token.kk == KK_STRLIT)) {
        nr_elements = token.u.text->length;
        if (nr_elements != type->nr_elements) ++nr_elements; /* auto NUL */
        output_string(token.u.text, nr_elements);
        lex();
    } else {
        if (outermost && !braced) error(ERROR_BADINIT);

        for (;;) {
            initialize(element_type, 0);
            ++nr_elements;

            if (type->nr_elements && (nr_elements == type->nr_elements)) 
                break;

            if (token.kk == KK_COMMA) {
                lex();
                prohibit(KK_RBRACE);
            } else
                break;
        }
    }

    if (braced) match(KK_RBRACE);

    if (type->nr_elements == 0) type->nr_elements = nr_elements;
    if ((type->nr_elements == 0) || (nr_elements > type->nr_elements)) error(ERROR_BADINIT);

    if (nr_elements < type->nr_elements) 
        output(" .fill %d,0\n", (type->nr_elements - nr_elements) * size_of(element_type));
}

static void
initialize_struct(struct type * type, int outermost)
{
    struct symbol * member;
    int             offset_bits = 0;
    int             adjust_bits;
    int             braced = 0;

    if (outermost) expect(KK_LBRACE);
    if (token.kk == KK_LBRACE) {
        ++braced;
        lex();
    }

    member = type->tag->list;
    if (type->tag->ss & S_UNION) error(ERROR_BADINIT);

    for (;;) {
        adjust_bits = (member->i * BITS) + T_GET_SHIFT(member->type->ts) - offset_bits;
        initialize_bits(0, adjust_bits % 8);
        if (adjust_bits / BITS) output(" .fill %d,0\n", adjust_bits / 8);
        offset_bits += adjust_bits;
        initialize(member->type, 0);

        if (member->type->ts & T_FIELD) 
            offset_bits += T_GET_SIZE(member->type->ts);
        else
            offset_bits += size_of(member->type) * 8;

        member = member->list;
        if (member == NULL) break;

        if (token.kk == KK_COMMA) {
            lex();
            prohibit(KK_RBRACE);
        } else
            break;
    }

    if (braced) match(KK_RBRACE);
    adjust_bits = (size_of(type) * BITS) - offset_bits;
    initialize_bits(0, adjust_bits % BITS);
    if (adjust_bits / BITS) output(" .fill %d,0\n", adjust_bits / 8);
}

static void
initialize(struct type * type, int outermost)
{
    if (type->ts & T_IS_SCALAR)
        initialize_scalar(type, outermost);
    else if (type->ts & T_ARRAY)
        initialize_array(type, outermost);
    else if (type->ts & T_TAG)
        initialize_struct(type, outermost);
    else
        error(ERROR_INTERNAL);
}

/* issue a zero initialization for the given symbol. */

static void
bss(struct symbol * symbol)
{
    output(".bss %G,%d,%d\n", symbol, size_of(symbol->type), align_of(symbol->type));
    symbol->ss &= ~S_TENTATIVE;
    symbol->ss |= S_DEFINED;
}

/* walk the symbol table and convert tentative definitions
   into actual definitions. */

static void
tentative1(struct symbol * symbol)
{
    if (symbol->ss & S_TENTATIVE) {
        if (complete_type(symbol->type)) {
            bss(symbol);
        } else {
            input_name = symbol->input_name;
            line_number = symbol->line_number;
            error(ERROR_TENTATIVE);
        }
    }
}

void
tentatives(void)
{
    walk_symbols(SCOPE_GLOBAL, SCOPE_GLOBAL, tentative1);
}

/* process a static initializer for 'symbol'. if 'braced'
   then the caller has encountered an opening brace (and
   will expect a matching closing brace)- it just helps 
   accommodate the rather haphazard brace-elision rules. */

static void 
static_initializer(struct symbol * symbol, int braced)
{
    struct block  * saved_block;

    saved_block = current_block;    /* don't allow code generation */
    current_block = NULL;
    segment(SEGMENT_DATA);
    output(".align %d\n", align_of(symbol->type));
    output("%G:", symbol);
    initialize(symbol->type, !braced);
    symbol->ss |= S_DEFINED;
    symbol->ss &= ~S_TENTATIVE;
    current_block = saved_block;
}

/* just declared the 'symbol' with the explicit storage class 'ss'.
   (we only care about 'ss' to distinguish between explicit and 
   implicit 'extern'). the job of initializer() is to process an 
   initializer, if present, or reserve uninitialized storage instead. */

void
initializer(struct symbol * symbol, int ss)
{
    struct tree   * tree;
    int             braced = 0;
    struct symbol * temp;

    if ((symbol->ss & (S_STATIC | S_EXTERN)) && !(ss & S_EXTERN)) {
        if (symbol->ss & S_STATIC) symbol->i = next_asm_label++;
        if (symbol->ss & S_EXTERN) symbol->ss |= S_REFERENCED;

        if (token.kk == KK_EQ) {
            if (symbol->ss & S_DEFINED) error(ERROR_DUPDEF);
            lex();
            static_initializer(symbol, 0);
        } else {
            if (symbol->scope != SCOPE_GLOBAL) 
                bss(symbol);
            else if (!(symbol->ss & S_DEFINED)) {
                symbol->ss |= S_TENTATIVE;
                symbol->input_name = input_name;
                symbol->line_number = line_number;
            }
        }
    } else if (symbol->ss & S_BLOCK) {
        if (token.kk == KK_EQ) {
            lex();

            if (token.kk == KK_LBRACE) {
                ++braced;
                lex();
            }

            if (symbol->type->ts & T_IS_SCALAR) {
                tree = symbol_tree(symbol);
                tree = assignment_expression(tree, ASSIGNMENT_CONST);
                generate(tree, GOAL_EFFECT, NULL);
            } else if (symbol->type->ts & (T_TAG | T_ARRAY)) {
                temp = new_symbol(NULL, S_STATIC, copy_type(symbol->type));
                put_symbol(temp, SCOPE_RETIRED);
                temp->i = next_asm_label++;
                static_initializer(temp, braced);
                compat_types(symbol->type, temp->type, COMPAT_TYPES_COMPOSE);
                choose(E_ASSIGN, memory_tree(symbol), memory_tree(temp));
            } else
                error(ERROR_INTERNAL);

            if (braced) match(KK_RBRACE);
        }

        size_of(symbol->type);  /* ensure locals are complete */
    }
}
