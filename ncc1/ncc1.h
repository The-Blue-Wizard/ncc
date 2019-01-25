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

/* let's start off with the basics. */

#define BITS                8

/* some useful generic macros. */

#define MIN(a,b)            (((a) < (b)) ? (a) : (b))
#define MAX(a,b)            (((a) > (b)) ? (a) : (b))
#define ROUND_UP(a,b)       (((a) % (b)) ? ((a) + ((b) - ((a) % (b)))) : (a))
#define ROUND_DOWN(a,b)     ((a) - ((a) % (b)))

/* same basic data about the activation records. probably
   shouldn't be changed, at least not without serious consideration. */

#define FRAME_ARGUMENTS     16      /* start of arguments in frame */
#define FRAME_ALIGN         8       /* always 8-byte aligned */

/* number of buckets in the hash tables. a power of two is preferable.
   more buckets can improve performance, but with NR_SYMBOL_BUCKETS in 
   particular, larger numbers can have a negative impact, as every bucket 
   must be scanned when exiting a scope. */

#define NR_STRING_BUCKETS   64
#define NR_SYMBOL_BUCKETS   32

/* limits the level of block nesting. the number is arbitrary, but
   it must be at least "a few less" than INT_MAX at most. */

#define SCOPE_MAX           1000

/*
 * MAX_SIZE limits the number of bytes specified by a type. 256MB - 1
 * is currently the largest safe value, due to the use of 'int' to 
 * store type sizes and the expansion from byte counts to bit counts
 * in certain places (notably the struct type code). increasing this
 * maximum doesn't seem to be worth the penalty in speed and complexity.
 */

#define MAX_SIZE            ((256 * 1024 * 1024) - 1)

#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include "token.h"
#include "tree.h"
#include "symbol.h"
#include "type.h"
#include "reg.h"
#include "block.h"
#include "peep.h"

extern int              g_flag;
extern int              O_flag;
extern FILE *           yyin;
extern struct token     token;
extern int              line_number;
extern struct string *  input_name;
extern struct string *  output_name;
extern FILE *           output_file;
extern int              current_scope;
extern int              next_asm_label;
extern int              next_iregister;
extern int              next_fregister;
extern int              loop_level;
extern struct symbol  * current_function;
extern struct tree    * return_struct_temp;
extern int              frame_offset;
extern int              save_iregs;
extern int              save_fregs;
extern struct block *   entry_block;
extern struct block *   current_block;
extern struct block *   exit_block;
extern struct block *   first_block;
extern struct block *   last_block;
extern struct symbol  * blkcpy_symbol;

extern void            error(int);
extern void            compound(void);
extern void            yyinit(void);
extern void            lex(void);
extern int             peek(struct string **);
extern void            expect(int);
extern void            match(int);
extern void            prohibit(int);
extern void            translation_unit(void);
extern void            local_declarations(void);
extern void            compute_global_defuses(void);
extern void            output(char *, ...);
extern void            output_string(struct string *, int);
extern void            output_function(void);
extern struct string * stringize(char *, int);
extern struct symbol * new_symbol(struct string *, int, struct type *);
extern void            free_symbol(struct symbol *);
extern struct symbol * find_symbol(struct string *, int, int, int);
extern struct symbol * find_typedef(struct string *);
extern struct symbol * find_label(struct string *);
extern struct symbol * find_symbol_list(struct string *, struct symbol **);
extern struct symbol * find_symbol_by_reg(int);
extern int             modifiable(struct type *);
extern int             complete_type(struct type *);
extern void            put_symbol(struct symbol *, int);
extern void            put_symbol_list(struct symbol *, struct symbol **);
extern void            free_symbol_list(struct symbol **);
extern void            store_symbol(struct symbol *);
extern void            free_symbols(void);
extern void            walk_symbols(int, int, void (*)(struct symbol *));
extern void            externs(void);
extern void            literals(void);
extern void            tentatives(void);
extern int             symbol_reg(struct symbol *);
extern struct proto  * new_proto(void);
extern struct proto  * copy_proto(struct proto *);
extern void            free_proto(struct proto *);
extern int             peek_type_specifier(void);
extern void            enter_scope(void);
extern void            exit_scope(int);
extern struct type   * argument_type(struct type *, int);
extern void            sequence_blocks(void);
extern void            setup_blocks(void);
extern void            free_block(struct block *);
extern void            free_blocks(void);
extern void            allocate_regs(void);
extern int             constant_expression(void);
extern struct tree   * assignment_expression(struct tree *, int);
extern struct tree   * fake_assignment(struct type *);
extern struct tree   * expression(void);
extern int             reg_is_dead(struct block *, struct insn *, int);
extern void            kill_insn(struct block *, struct insn *);
extern struct insn   * new_insn(int, ...);
extern int             insn_uses_reg(struct insn *, int);
extern int             insn_defs_reg(struct insn *, int);
extern int             insn_nr_defs(struct insn *);
extern int             insn_nr_uses(struct insn *);
extern int             insn_touches_reg(struct insn *, int);
extern void            insn_replace_reg(struct insn *, int, int);
extern int             peep_match(struct block *, struct insn *, struct peep_match *);
extern void            free_tree(struct tree *);
extern void            optimize(void);
extern struct tree   * temporary(struct type *);
extern struct symbol * temporary_symbol(struct type *);
extern struct symbol * string_symbol(struct string *);
extern void            validate_type(struct type *);
extern void            merge_qualifiers(struct type *, struct type *);
extern int             choose(int, struct tree *, struct tree *);
extern struct tree   * int_tree(int, long);
extern struct tree   * float_tree(int, double);
extern struct tree   * float_literal(struct tree *);
extern struct block  * new_block(void);
extern struct tree   * memory_tree(struct symbol *);
extern void            put_insn(struct block *, struct insn *, struct insn *);
extern void            succeed_block(struct block *, int, struct block *);
extern void            unsucceed_block(struct block *, int);
extern int             block_successor_cc(struct block *, int);
extern struct block  * block_successor(struct block *, int);
extern struct block  * block_predecessor(struct block *, int);
extern void            split_block(struct block *);
extern int             size_of(struct type *);
extern int             align_of(struct type *);
extern void            segment(int);
extern struct type   * new_type(int);
extern struct type   * copy_type(struct type *);
extern void            free_type(struct type *);
extern void            compat_types(struct type *, struct type *, int);
extern struct defuse * find_defuse(struct block *, int, int);
extern struct defuse * find_defuse_by_symbol(struct block *, struct symbol *);
extern struct type   * abstract_type(void);
extern struct tree   * new_tree(int op, struct type * type, ...);
extern struct tree   * copy_tree(struct tree *);
extern struct tree   * conditional_expression(void);
extern void          * allocate(int);
extern void            decap_tree(struct tree *, struct type **, struct tree **, struct tree **, struct tree **);
extern struct tree   * reg_tree(int, struct type *);
extern struct tree   * stack_tree(struct type *, int);
extern struct tree   * symbol_tree(struct symbol *);
extern struct type   * splice_types(struct type *, struct type *);
extern struct tree   * scalar_expression(struct tree *);
extern struct tree   * addr_tree(struct tree *);
extern void            commute_tree(struct tree *);
extern void            compat_types(struct type *, struct type *, int);
extern void            initializer(struct symbol *, int);
extern struct tree   * generate(struct tree *, int, int *);

#ifndef NDEBUG
extern void             debug_type(struct type *);
extern void             debug_tree(struct tree *);
#endif

/* mode for exit_scope() */

#define EXIT_SCOPE_BLOCK        0
#define EXIT_SCOPE_PROTO        1

/* flags for assignment_expression() */

#define ASSIGNMENT_CONST        0x00000001

/* goals for generate() */

#define GOAL_EFFECT             0
#define GOAL_CC                 1
#define GOAL_VALUE              2

/* modes for find_defuse() */

#define FIND_DEFUSE_NORMAL      0
#define FIND_DEFUSE_CREATE      1

/* flags for declarations() */

#define DECLARATIONS_ARGS       0x00000001
#define DECLARATIONS_INTS       0x00000002 
#define DECLARATIONS_FIELDS     0x00000004

/* flags for compat_types() */

#define COMPAT_TYPES_COMPOSE    0x00000001
#define COMPAT_TYPES_QUALS      0x00000002
#define COMPAT_TYPES_ASSIGN     0x00000004

/* mode for argument_type() */

#define ARGUMENT_TYPE_OLD       0
#define ARGUMENT_TYPE_NEW       1

/* output segments */

#define SEGMENT_TEXT    0           /* code */
#define SEGMENT_DATA    1           /* initialized data */

/* these codes must match the indices of errors[] in cc1.c */

#define ERROR_CMDLINE       0       /* bad command line */
#define ERROR_INPUT         1       /* input file error */
#define ERROR_SYNTAX        2       /* syntax error */
#define ERROR_MEMORY        3       /* out of memory */
#define ERROR_DIRECTIVE     4       /* bad # directive */
#define ERROR_LEXICAL       5       /* invalid character in input */
#define ERROR_BADICON       6       /* malformed integral constant */
#define ERROR_IRANGE        7       /* integral constant out of range */
#define ERROR_BADFCON       8       /* malformed floating constant */
#define ERROR_FRANGE        9       /* floating constant out of range */
#define ERROR_ESCAPE        10      /* invalid octal escape sequence */
#define ERROR_UNTERM        11      /* unterminated string literal */
#define ERROR_BADCCON       12      /* invalid character constant */
#define ERROR_CRANGE        13      /* multi-character constant out of range */
#define ERROR_OUTPUT        14      /* output file error */
#define ERROR_NESTING       15      /* nesting level too deep */
#define ERROR_TYPESIZE      16      /* type too big */
#define ERROR_INCOMPLT      17      /* incomplete type */
#define ERROR_ILLFUNC       18      /* illegal use of function type */
#define ERROR_ILLARRAY      19      /* illegal array specification */
#define ERROR_RETURN        20      /* illegal function return type */
#define ERROR_STRUCT        21      /* illegal use of struct/union type */
#define ERROR_NOARGS        22      /* misplaced formal arguments */
#define ERROR_ILLFIELD      23      /* illegal use of bit field */
#define ERROR_FIELDTY       24      /* illegal bit field type */
#define ERROR_FIELDSZ       25      /* invalid bit field size */
#define ERROR_SCLASS        26      /* storage class not permitted */
#define ERROR_TAGREDEF      27      /* struct/union already defined */
#define ERROR_EMPTY         28      /* empty struct or union */
#define ERROR_REMEMBER      29      /* duplicate member declaration */
#define ERROR_REDECL        30      /* illegal redeclaration */
#define ERROR_NOTARG        31      /* unknown argument identifier */
#define ERROR_TYPEDEF       32      /* can't do that with typedef */
#define ERROR_UNKNOWN       33      /* unknown identifier */
#define ERROR_OPERANDS      34      /* illegal operands */
#define ERROR_INCOMPAT      35      /* incompatible operands */
#define ERROR_LVALUE        36      /* not an lvalue */
#define ERROR_ABSTRACT      37      /* abstract declarator required */
#define ERROR_MISSING       38      /* declarator missing identifier */
#define ERROR_BADCAST       39      /* bad typecast */
#define ERROR_INDIR         40      /* illegal indirection */
#define ERROR_NOTSTRUCT     41      /* left side must be struct */
#define ERROR_NOTMEMBER     42      /* not a member of that struct/union */
#define ERROR_NEEDFUNC      43      /* function type required */
#define ERROR_INTERNAL      44      /* compiler internal error */
#define ERROR_REGISTER      45      /* can't take address of register */
#define ERROR_CONEXPR       46      /* constant expression required */
#define ERROR_DIV0          47      /* division by zero */
#define ERROR_BADEXPR       48      /* illegal constant expression */
#define ERROR_BADINIT       49      /* bad initializer */
#define ERROR_DUPDEF        50      /* duplicate definition */
#define ERROR_MISPLACED     51      /* misplaced break, continue or case */
#define ERROR_DANGLING      52      /* undefined label */
#define ERROR_DUPCASE       53      /* duplicate case label */
#define ERROR_CASE          54      /* switch/case must be integral */
#define ERROR_TAGMATCH      55      /* tag class mismatch */
#define ERROR_TENTATIVE     56      /* tentative definition incomplete */
#define ERROR_DUPQUAL       57      /* duplicate type qualifier */
#define ERROR_DUPCLASS      58      /* duplicate storage class */
#define ERROR_DUPSPEC       59      /* duplicate type specifier */
#define ERROR_ILLTYPE       60      /* illegal type specification */
#define ERROR_ASSCONST      61      /* assignment to const */
#define ERROR_DISQUAL       62      /* discards qualifiers */
#define ERROR_ILLVOID       63      /* illegal use of void type */
#define ERROR_SCALAR        64      /* scalar expression required */
#define ERROR_VARIADIC      65      /* illegal variadic function */
#define ERROR_DUPARG        66      /* duplicate argument name */
#define ERROR_ARGNAME       67      /* missing argument name */
#define ERROR_ARGCOUNT      68      /* incorrect number of arguments */

