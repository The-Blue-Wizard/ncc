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
#include <string.h>
#include <stdarg.h>
#include "ncc1.h"

/* constant expressions must evaluate to an int */

int
constant_expression(void)
{
    struct tree * tree;
    long          value;

    tree = conditional_expression();
    tree = generate(tree, GOAL_VALUE, NULL);
    if (tree->op != E_CON) error(ERROR_CONEXPR);
    if (!(tree->type->ts & T_INT)) error(ERROR_BADEXPR);
    value = tree->u.con.i;
    free_tree(tree);
    return value;
}

/* create a new tree node with op 'op' of type 'type' and up to 
   three children. the ownership of 'type' is taken by the node. */

struct tree *
new_tree(int op, struct type * type, ...)
{
    struct tree * tree;
    va_list       args;

    tree = (struct tree *) allocate(sizeof(struct tree));
    tree->op = op;
    tree->type = type;
    tree->list = NULL;

    if (E_IS_LEAF(op)) {
        switch (op)
        {
        case E_NOP:
        case E_CON:
            break;
        case E_SYM:
            tree->u.sym = NULL;
            break;
        case E_REG:
            tree->u.reg = R_NONE;
            break;
        case E_IMM:
        case E_MEM:
            tree->u.mi.glob = NULL;
            tree->u.mi.ofs = 0;
            tree->u.mi.b = R_NONE;
            tree->u.mi.i = R_NONE;
            tree->u.mi.s = 1;
            tree->u.mi.rip = 0;
            break;
        }
    } else {
        va_start(args, type);
        tree->u.ch[0] = E_HAS_CH0(op) ? va_arg(args, struct tree *) : NULL;
        tree->u.ch[1] = E_HAS_CH1(op) ? va_arg(args, struct tree *) : NULL;
        tree->u.ch[2] = E_HAS_CH2(op) ? va_arg(args, struct tree *) : NULL;
        va_end(args);
    }

    return tree;
}

/* free a tree and any forest headed by this tree. */

void
free_tree(struct tree * tree)
{
    int i;

    if (tree) {
        if (!E_IS_LEAF(tree->op))
            for (i = 0; i < NR_TREE_CH; i++) 
                free_tree(tree->u.ch[i]);

        free_tree(tree->list);
        free_type(tree->type);
        free(tree);
    }
}

/* decapitate a tree. the tree's type and children are preserved and given
   to the caller, if it expresses interest by passing in valid pointers to
   capture them. then the top of the tree is freed. */

void
decap_tree(struct tree * tree, struct type ** type, struct tree ** ch0, struct tree ** ch1, struct tree ** ch2)
{
    if (type) { 
        *type = tree->type;
        tree->type = NULL;
    }

    if (ch0) {
        *ch0 = tree->u.ch[0];
        tree->u.ch[0] = NULL;
    }

    if (ch1) {
        *ch1 = tree->u.ch[1];
        tree->u.ch[1] = NULL;
    }

    if (ch2) {
        *ch2 = tree->u.ch[2];
        tree->u.ch[2] = NULL;
    }

    free_tree(tree);
}

/* copy this tree/forest */

struct tree *
copy_tree(struct tree * tree)
{
    struct tree * tree2 = NULL;
    int           i;

    if (tree) {
        tree2 = new_tree(tree->op, copy_type(tree->type), NULL, NULL, NULL);
        tree2->list = copy_tree(tree->list);

        if (E_IS_LEAF(tree->op)) 
            memcpy(&(tree2->u), &(tree->u), sizeof(tree->u));
        else {
            for (i = 0; i < NR_TREE_CH; i++) 
                tree2->u.ch[i] = copy_tree(tree->u.ch[i]);
        }
    }

    return tree2;
}

/* create an E_SYM node that references 'symbol'. must be
   used rather than creating the node manually to be sure 
   we catch all referenced S_EXTERNs. */

struct tree *
symbol_tree(struct symbol * symbol)
{
    struct tree * tree;

    tree = new_tree(E_SYM, copy_type(symbol->type));
    tree->u.sym = symbol;
    symbol->ss |= S_REFERENCED;
    return tree;
}

/* return an E_REG tree. takes ownership of 'type'. */

struct tree *
reg_tree(int reg, struct type * type)
{
    struct tree * tree;

    tree = new_tree(E_REG, type);
    tree->u.reg = reg;

    return tree;
}


/* create an E_MEM tree that references the memory allocated to 'symbol'.
   for local (S_BLOCK) symbols, this will result in an [RBP+x].
   for S_STATIC or S_EXTERN symbols, this will yield [RIP x]. */

struct tree * 
memory_tree(struct symbol * symbol)
{
    struct tree * tree;

    tree = new_tree(E_MEM, copy_type(symbol->type));

    if (symbol->ss & S_BLOCK) {
        store_symbol(symbol);
        tree->u.mi.ofs = symbol->i;
        tree->u.mi.b = R_BP;
    } else {
        tree->u.mi.glob = symbol;
        tree->u.mi.rip = 1;
    } 

    return tree;
}

/* int_tree(), float_tree() are convenience functions */

struct tree *
int_tree(int ts, long i)
{
    struct tree * tree;

    tree = new_tree(E_CON, new_type(ts));
    tree->u.con.i = i;
    return tree;
}

struct tree *
float_tree(int ts, double f)
{
    struct tree * tree;

    tree = new_tree(E_CON, new_type(ts));
    tree->u.con.f = f;
    return tree;
}

/* return an E_MEM that refers to a location on the stack.
   as usual, the caller yields ownership of the 'type'. */

struct tree *
stack_tree(struct type * type, int offset)
{
    struct tree * tree;

    tree = new_tree(E_MEM, type);
    tree->u.mi.b = R_SP;
    tree->u.mi.ofs = offset;

    return tree;
}

/* take the address of the tree. obviously must be an lvalue. */

struct tree *
addr_tree(struct tree * tree)
{
    if ((tree->op == E_SYM) && (tree->u.sym->ss & S_LOCAL)) {
        tree->u.sym->ss &= ~S_LOCAL;
        tree->u.sym->ss |= S_AUTO;
    }

    return new_tree(E_ADDR, splice_types(new_type(T_PTR), copy_type(tree->type)), tree);
}

/* send tree to work .. (paying attention?) */

void
commute_tree(struct tree * tree)
{
    struct tree * tmp;

    tmp = tree->u.ch[0];
    tree->u.ch[0] = tree->u.ch[1];
    tree->u.ch[1] = tmp;
}

/* lvalues are either symbols or indirections. the front end 
   will even generate an E_REG node in return_statement(). */

static void
lvalue(struct tree * tree)
{
    if ((tree->op != E_SYM) && (tree->op != E_FETCH) && (tree->op != E_REG))
        error(ERROR_LVALUE);
}

/* perform standard promotions on a tree. this is called in most
   circumstances before a tree is used as an operand. it will:
   1. convert all char or short to int,
   2. convert array into rvalue pointer to its first element,
   3. convert function to pointer-to-function. */

static struct tree *
promote(struct tree * tree)
{
    struct type * type;

    if (tree->type->ts & (T_IS_CHAR | T_IS_SHORT))
        return new_tree(E_CAST, new_type(T_INT), tree);

    if (tree->type->ts & T_FUNC) return addr_tree(tree);

    if (tree->type->ts & T_ARRAY) {
        type = copy_type(tree->type->next); /* element type */
        tree = addr_tree(tree);
        return new_tree(E_CAST, splice_types(new_type(T_PTR), type), tree);
    }

    return tree;
}

/* make sure the tree is a scalar */

struct tree *
scalar_expression(struct tree * tree)
{
    if (!(tree->type->ts & T_IS_SCALAR)) tree = promote(tree);
    if (!(tree->type->ts & T_IS_SCALAR)) error(ERROR_SCALAR);

    return tree;
}

/* perform the usual conversions to bring the operands to common type,
   if they're arithmetic. perhaps unexpectedly, this is the function
   responsible for filling in the type of the result for most binary
   operators (and the ternary operator).
   
   this function requires that the bits in type.h T_* be correctly ordered. */

static void
usuals(struct tree * tree)
{
    struct tree * left = tree->u.ch[0];
    struct tree * right = tree->u.ch[1];
    int           ts;

    if ((left->type->ts & T_IS_ARITH) && (right->type->ts & T_IS_ARITH)) {
        for (ts = T_LDOUBLE; ts != T_INT; ts >>= 1) {
            if ((left->type->ts & ts) || (right->type->ts & ts))
                break;
        }

        if (!(left->type->ts & ts)) left = new_tree(E_CAST, new_type(ts), left);
        if (!(right->type->ts & ts)) right = new_tree(E_CAST, new_type(ts), right);
        tree->u.ch[0] = left;
        tree->u.ch[1] = right;
    }

    switch (tree->op)
    {
    case E_EQ:
    case E_NEQ:
    case E_GT:
    case E_LT:
    case E_GTEQ:
    case E_LTEQ:
        tree->type = new_type(T_INT);
        break;
    default:
        if (right->type->ts & T_PTR) 
            tree->type = copy_type(right->type);
        else
            tree->type = copy_type(left->type);
    }
}

/* if the top of 'tree' is E_ADD or E_SUB involving pointer types, fix up
   the tree to account for pointer scaling. */

static struct tree *
scale_pointers(struct tree * tree)
{
    long scale_factor; /* long to avoid casts to int_tree() */

    if (!(tree->type->ts & T_PTR)) return tree;
    if ((tree->op != E_ADD) && (tree->op != E_SUB)) return tree;

    /* normalize (int + ptr) to (ptr + int). 
       (int - ptr) is not permitted */

    if (tree->u.ch[0]->type->ts & T_IS_INTEGRAL) commute_tree(tree);

    scale_factor = size_of(tree->u.ch[0]->type->next);

    if (tree->u.ch[1]->type->ts & T_IS_INTEGRAL) { /* (ptr + int) or (int + ptr) */
        tree->u.ch[1] = new_tree(E_CAST, new_type(T_LONG), tree->u.ch[1]);
        tree->u.ch[1] = new_tree(E_MUL, new_type(T_LONG), tree->u.ch[1], int_tree(T_LONG, scale_factor));
    } else { /* (ptr - ptr) */
        tree = new_tree(E_CAST, new_type(T_LONG), tree);
        tree = new_tree(E_DIV, new_type(T_LONG), tree, int_tree(T_LONG, scale_factor));
        tree = new_tree(E_CAST, new_type(T_INT), tree);
    }

    return tree;
}

/* if 'tree' represents the integral constant 0 and 'type' is a pointer,
   then convert the tree to a null pointer of 'type'. */

static struct tree *
null_pointer(struct tree * tree, struct type * type)
{
    if ((type->ts & T_PTR) && (tree->op == E_CON) && (tree->u.con.i == 0)) 
        tree = new_tree(E_CAST, copy_type(type), tree);
    
    return tree;
}

/* if the type of 'tree' and 'type' are both pointer types, then:

    if mode == VOID_POINTER_VOID:
        if 'type' is a void *, and 'tree' is not, then
        'tree' is cast to 'type'.

    if mode == VOID_POINTER_OTHER:
        if 'tree' is a void *, and 'type' is not, then
        'tree' is cast to 'type'.

   in either case, the qualifiers of the type pointed to by 'tree'
   are preserved. */

#define VOID_POINTER_VOID   0
#define VOID_POINTER_OTHER  1

static struct tree *
void_pointer(struct tree * tree, struct type * type, int mode)
{
    struct type * tree_target;
    struct type * type_target;
    int           qual_ts;

    if (!(tree->type->ts & T_PTR) || !(type->ts & T_PTR)) return tree;

    tree_target = tree->type->next;
    type_target = type->next;
    qual_ts = tree_target->ts & T_QUAL_MASK;

    if (        ((mode == VOID_POINTER_VOID) 
            &&  (tree_target->ts & T_VOID) 
            &&  !(type_target->ts & T_VOID))
        ||
                ((mode == VOID_POINTER_OTHER)
            &&  (type_target->ts & T_VOID)
            &&  !(tree_target->ts & T_VOID)) )
    {
        tree = new_tree(E_CAST, copy_type(type), tree);
        tree->type->next->ts |= qual_ts;
        return tree;
    }

    return tree;
}

/* check that 'left' and 'right' types are acceptable to binary operator 'op'.
   note that null_pointer() must be called (if allowed/necessary) before this. */

static void
check_operand_types(int op, struct type * left, struct type * right)
{
    if ((op != E_LOR) && (op != E_LAND) && (left->ts & right->ts & T_PTR))
        compat_types(left, right, (op == E_ASSIGN) ? COMPAT_TYPES_ASSIGN : 0);

    switch (op)
    {   
    case E_SHL:
    case E_SHR:
    case E_MOD:
    case E_AND:
    case E_OR:
    case E_XOR:
        if (!(left->ts & T_IS_INTEGRAL) || !(right->ts & T_IS_INTEGRAL)) 
            error(ERROR_INCOMPAT);
        else
            break;

    case E_MUL:
    case E_DIV:
        if (!(left->ts & T_IS_ARITH) || !(right->ts & T_IS_ARITH)) 
            error(ERROR_INCOMPAT);
        else 
            break;

    case E_TERN:
        if (    (!(left->ts & T_IS_ARITH) || !(right->ts & T_IS_ARITH))
            &&  (!(left->ts & right->ts & T_PTR)) 
            &&  (!(left->ts & right->ts & T_VOID))
            &&  (!((left->ts & right->ts & T_TAG) && (left->tag == right->tag))))
        {
            error(ERROR_INCOMPAT);
        } else
            break;

    case E_ASSIGN:
        if (    (!(left->ts & T_IS_ARITH) || !(right->ts & T_IS_ARITH))
            &&  (!(left->ts & right->ts & T_PTR)) 
            &&  (!((left->ts & right->ts & T_TAG) && (left->tag == right->tag))))
         {
            error(ERROR_INCOMPAT);
         } else
            break;

    case E_EQ:
    case E_NEQ:
    case E_GT:
    case E_GTEQ:
    case E_LT:
    case E_LTEQ:
        if (    (!(left->ts & T_IS_ARITH) || !(right->ts & T_IS_ARITH))
            &&  (!(left->ts & right->ts & T_PTR)) )
        {
            error(ERROR_INCOMPAT);
        } else
            break;

    case E_SUB:
        if (    (!(left->ts & T_IS_ARITH) || !(right->ts & T_IS_ARITH))
            &&  (!(left->ts & right->ts & T_PTR))   
            &&  (!(left->ts & T_PTR) || !(right->ts & T_IS_INTEGRAL)) )
        {
            error(ERROR_INCOMPAT);
        } else
            break;

    case E_ADD:
        if (    (!(left->ts & T_IS_ARITH) || !(right->ts & T_IS_ARITH))
            &&  (!(left->ts & T_IS_INTEGRAL) || !(right->ts & T_PTR)) 
            &&  (!(left->ts & T_PTR) || !(right->ts & T_IS_INTEGRAL)) )
        {
            error(ERROR_INCOMPAT);
        } else
            break;

    case E_LOR:
    case E_LAND:
        if (!(left->ts & T_IS_SCALAR) || !(right->ts & T_IS_SCALAR)) 
            error(ERROR_INCOMPAT);
        else
            break;
    }
}

/* given an operator (token) and two operands, fashion a tree,
   enforcing C semantics like type compatibility, pointer scaling, etc. 
   'condition' is ignored except for KK_QUEST (the ternary operator). */

static struct tree *
token_tree(int kk, struct tree * left, struct tree * right, struct tree * condition)
{
    int           op;
    int           null_zeroes;
    int           do_usuals;

    switch (kk)
    {
    case KK_PLUS:   do_usuals = 1; null_zeroes = 0; op = E_ADD; break;
    case KK_MINUS:  do_usuals = 1; null_zeroes = 0; op = E_SUB; break;
    case KK_STAR:   do_usuals = 1; null_zeroes = 0; op = E_MUL; break;
    case KK_DIV:    do_usuals = 1; null_zeroes = 0; op = E_DIV; break;
    case KK_MOD:    do_usuals = 1; null_zeroes = 0; op = E_MOD; break;
    case KK_SHL:    do_usuals = 1; null_zeroes = 0; op = E_SHL; break;
    case KK_SHR:    do_usuals = 1; null_zeroes = 0; op = E_SHR; break;
    case KK_AND:    do_usuals = 1; null_zeroes = 0; op = E_AND; break;
    case KK_BAR:    do_usuals = 1; null_zeroes = 0; op = E_OR; break;
    case KK_XOR:    do_usuals = 1; null_zeroes = 0; op = E_XOR; break;
    case KK_GT:     do_usuals = 1; null_zeroes = 1; op = E_GT; break;
    case KK_LT:     do_usuals = 1; null_zeroes = 1; op = E_LT; break;
    case KK_GTEQ:   do_usuals = 1; null_zeroes = 1; op = E_GTEQ; break;
    case KK_LTEQ:   do_usuals = 1; null_zeroes = 1; op = E_LTEQ; break;
    case KK_EQEQ:   do_usuals = 1; null_zeroes = 1; op = E_EQ; break;
    case KK_BANGEQ: do_usuals = 1; null_zeroes = 1; op = E_NEQ; break;
    case KK_BARBAR: do_usuals = 0; null_zeroes = 0; op = E_LOR; break;
    case KK_ANDAND: do_usuals = 0; null_zeroes = 0; op = E_LAND; break;
    case KK_QUEST:  do_usuals = 1; null_zeroes = 1; op = E_TERN; break;
    }

    left = promote(left);
    right = promote(right);
        
    if (null_zeroes) {
        left = null_pointer(left, right->type);
        right = null_pointer(right, left->type);
    }

    check_operand_types(op, left->type, right->type);
    left = new_tree(op, NULL, left, right, (op == E_TERN) ? condition : NULL);

    if (do_usuals) 
        usuals(left);
    else
        left->type = new_type(T_INT);

    left = scale_pointers(left);

    return left;
}

/* peek into the future and return true if it looks like
   a type specifier, or false otherwise.

   note that storage classes aren't type specifiers, but 
   they might appear in prototype arguments, so we call
   them type specifiers for our purposes here. any illegal
   short classes will be caught by the parser later. */

int
peek_type_specifier(void)
{
    struct string * id;
    int             kk;

    kk = peek(&id);
    switch (kk) {
    case KK_IDENT:
        if (!find_typedef(id)) break;
        /* fall through */
    case KK_STRUCT:
    case KK_UNION:
    case KK_ENUM:
    case KK_CHAR:
    case KK_SHORT:
    case KK_SIGNED:
    case KK_INT:
    case KK_LONG:
    case KK_FLOAT:
    case KK_DOUBLE:
    case KK_UNSIGNED:
    case KK_VOID:
    case KK_CONST:
    case KK_VOLATILE:
    case KK_REGISTER:
    case KK_AUTO:
    case KK_TYPEDEF:
    case KK_EXTERN:
    case KK_STATIC:
        return 1;
    }

    return 0;
}

/* generate a tree to increment or decrement the given tree.
   'op' is either E_POST or E_PRE, depending on whether it's
   a postfix or prefix operator. kk is either KK_INC or KK_DEC,
   indicating an increment or decrement operation. 

   this function is responsible for ensuring the passed tree
   is appropriate for the operation, so callers need not worry. */

static struct tree *
pre_or_post(struct tree * tree, int op, int kk)
{
    struct tree * one;

    lvalue(tree);
    one = int_tree(T_INT, 1L);
    if (kk == KK_DEC) one->u.con.i = -1;
    if (!(tree->type->ts & T_IS_SCALAR)) error(ERROR_OPERANDS);
    if (tree->type->ts & T_IS_ARITH) one = new_tree(E_CAST, copy_type(tree->type), one);
    tree = new_tree(E_ADD, copy_type(tree->type), tree, one);
    tree = scale_pointers(tree);
    tree->op = op;
}

/* the rest of the file comprises a conventional recursive-descent
   parser which generates trees from C expression. the only thing 
   remotely fancy is binary_expression(), which takes advantage of
   the common associativity of the middle precedence levels. */

static struct tree * cast_expression(void);

static struct tree *
primary_expression(void)
{
    struct symbol * symbol;
    struct tree   * tree;
    struct string * id;
    int             ts;

    switch (token.kk)
    {
    case KK_LPAREN:
        match(KK_LPAREN);
        tree = expression();
        match(KK_RPAREN);
        return tree;

    case KK_IDENT:
        id = token.u.text;
        lex();
        symbol = find_symbol(id, S_NORMAL, SCOPE_GLOBAL, current_scope);

        if (symbol == NULL) {
            if (token.kk == KK_LPAREN) {
                symbol = new_symbol(id, S_EXTERN | S_LURKER, splice_types(new_type(T_FUNC), new_type(T_INT)));
                put_symbol(symbol, SCOPE_GLOBAL);
            } else 
                error(ERROR_UNKNOWN);
        }

        if (symbol->ss & S_TYPEDEF) error(ERROR_TYPEDEF);
        if (symbol->ss & S_CONST) return int_tree(T_INT, (long) symbol->i);
        return symbol_tree(symbol);

    case KK_STRLIT:
        symbol = string_symbol(token.u.text);
        lex();
        return symbol_tree(symbol);

    case KK_ICON:   tree = int_tree(T_INT, token.u.i); lex(); return tree;
    case KK_UCON:   tree = int_tree(T_UINT, token.u.i); lex(); return tree;
    case KK_LCON:   tree = int_tree(T_LONG, token.u.i); lex(); return tree;
    case KK_ULCON:  tree = int_tree(T_ULONG, token.u.i); lex(); return tree;
    case KK_FCON:   tree = float_tree(T_FLOAT, token.u.f); lex(); return tree;
    case KK_DCON:   tree = float_tree(T_DOUBLE, token.u.f); lex(); return tree;
    case KK_LDCON:  tree = float_tree(T_LDOUBLE, token.u.f); lex(); return tree;

    default:
        error(ERROR_SYNTAX);
    }
}

static struct tree *
member_expression(struct tree * tree)
{
    struct symbol * tag;
    struct symbol * member;
    struct type   * type;

    if (token.kk == KK_DOT) {
        lvalue(tree);
        tree = addr_tree(tree);
    }

    if (!(tree->type->ts & T_PTR)) error(ERROR_INDIR);
    if (!(tree->type->next->ts & T_TAG)) error(ERROR_NOTSTRUCT);
    tag = tree->type->next->tag;
    if (!(tag->ss & S_DEFINED)) error(ERROR_INCOMPLT);
    lex();
    expect(KK_IDENT);
    member = find_symbol_list(token.u.text, &(tag->list));
    if (!member) error(ERROR_NOTMEMBER);
    lex();
    type = splice_types(new_type(T_PTR), copy_type(member->type));
    tree = new_tree(E_ADD, type, tree, int_tree(T_LONG, (long) member->i) );
    return new_tree(E_FETCH, copy_type(tree->type->next), tree);
}

/* process actual function arguments. these come in two varieties, of 
   course: arguments that are matched with formal prototyped arguments,
   and those that are not. the rules are obviously different, and the 
   two may be mixed in one call (think: variadic functions), hence two 
   different functions. */

static void
old_actual_arguments(struct tree * tree)
{
    struct tree * argument;

    while (token.kk != KK_RPAREN) {
        argument = assignment_expression(NULL, 0);
        argument = promote(argument);

        if (argument->type->ts & T_FLOAT)
            argument = new_tree(E_CAST, new_type(T_DOUBLE), argument);

        if (argument->type->ts & T_VOID) error(ERROR_ILLVOID);

        argument->list = tree->u.ch[1];
        tree->u.ch[1] = argument;

        if (token.kk == KK_COMMA) {
            lex();
            prohibit(KK_RPAREN);
        } else
            break;
    }
}

static void
new_actual_arguments(struct proto * proto, struct tree * tree)
{
    struct symbol * proto_arg;
    struct tree   * argument;

    proto_arg = proto->args;

    while (token.kk != KK_RPAREN) {
        if (proto_arg == NULL) {
            if (!(proto->ps & P_VARIADIC)) 
                error(ERROR_ARGCOUNT);
            else
                break;
        }

        argument = fake_assignment(proto_arg->type);
        argument->list = tree->u.ch[1];
        tree->u.ch[1] = argument;
        proto_arg = proto_arg->list;

        if (token.kk == KK_COMMA) {
            lex();
            prohibit(KK_RPAREN);
        } else
            break;
    }

    if (proto_arg) error(ERROR_ARGCOUNT);
    if (proto->ps & P_VARIADIC) old_actual_arguments(tree);
}

static struct tree *
call_expression(struct tree * tree)
{
    struct tree  * argument;
    struct proto * proto;

    tree = promote(tree);

    if ((!(tree->type->ts & T_PTR)) || (!(tree->type->next->ts & T_FUNC))) 
        error(ERROR_NEEDFUNC);

    proto = tree->type->next->proto;
    tree = new_tree(E_CALL, copy_type(tree->type->next->next), tree, NULL);
    match(KK_LPAREN);

    if (proto) 
        new_actual_arguments(proto, tree);
    else
        old_actual_arguments(tree);

    match(KK_RPAREN);
    return tree;
}

static struct tree *
postfix_expression(void)
{
    struct tree * tree;
    struct tree * index;

    tree = primary_expression();
    for (;;) {
        switch (token.kk) {
        case KK_ARROW:
        case KK_DOT:
            tree = member_expression(tree);
            break;
        case KK_INC:
        case KK_DEC:
            tree = pre_or_post(tree, E_POST, token.kk);
            lex();
            break;
        case KK_LPAREN:
            tree = call_expression(tree);
            break;
        case KK_LBRACK:
            lex();
            index = expression();
            tree = token_tree(KK_PLUS, tree, index, NULL);
            if (!(tree->type->ts & T_PTR)) error(ERROR_INDIR);
            tree = new_tree(E_FETCH, copy_type(tree->type->next), tree);
            match(KK_RBRACK);
            break;

        default:
            return tree;
        }
    }
}

static struct tree *
unary_expression(void)
{
    struct type * type;
    struct tree * tree;
    int           size;
    int           kk;

    switch (token.kk) {
    case KK_SIZEOF:
        lex();
        if ((token.kk == KK_LPAREN) && peek_type_specifier()) {
            lex();
            type = abstract_type();
            match(KK_RPAREN);
            size = size_of(type);
            free_type(type);
        } else {
            tree = unary_expression();
            if (tree->type->ts & T_FIELD) error(ERROR_ILLFIELD);
            size = size_of(tree->type);
            free_tree(tree);
        }

        return int_tree(T_INT, (long) size);

    case KK_INC:
    case KK_DEC:
        kk = token.kk;
        lex();
        return pre_or_post(unary_expression(), E_PRE, kk);

    case KK_BANG:
        lex();
        tree = cast_expression();
        tree = promote(tree);
        if (!(tree->type->ts & T_IS_SCALAR)) error(ERROR_OPERANDS);
        return new_tree(E_NOT, new_type(T_INT), tree);
        
    case KK_AND:
        lex();
        tree = cast_expression();
        lvalue(tree);
        if ((tree->op == E_SYM) && (tree->u.sym->ss & S_REGISTER)) error(ERROR_REGISTER);
        if (tree->type->ts & T_FIELD) error(ERROR_ILLFIELD);
        return addr_tree(tree);

    case KK_STAR:
        lex();
        tree = cast_expression();
        tree = promote(tree);
        if (!(tree->type->ts & T_PTR)) error(ERROR_INDIR);
        if (tree->type->next->ts & T_VOID) error(ERROR_ILLVOID);
        return new_tree(E_FETCH, copy_type(tree->type->next), tree);

    case KK_MINUS:
        lex();
        tree = cast_expression();
        tree = promote(tree);
        if (!(tree->type->ts & T_IS_ARITH)) error(ERROR_OPERANDS);
        return new_tree(E_NEG, copy_type(tree->type), tree);

    case KK_TILDE:
        lex();
        tree = cast_expression();
        tree = promote(tree);
        if (!(tree->type->ts & T_IS_INTEGRAL)) error(ERROR_OPERANDS);
        return new_tree(E_COM, copy_type(tree->type), tree);

    default:
        return postfix_expression();
    }
}

static struct tree *
cast_expression(void)
{
    struct type *   type;
    struct tree *   tree;

    if ((token.kk == KK_LPAREN) && peek_type_specifier()) {
        lex();
        type = abstract_type();
        match(KK_RPAREN);
        tree = cast_expression();
        tree = promote(tree);

        if (    !(type->ts & (T_IS_SCALAR | T_VOID)) 
            ||  !(tree->type->ts & (T_IS_SCALAR | T_VOID))
            ||  ((tree->type->ts & T_PTR) && (type->ts & T_IS_FLOAT))
            ||  ((tree->type->ts & T_IS_FLOAT) && (type->ts & T_PTR)) )
        {
            error(ERROR_BADCAST);
        }
        return new_tree(E_CAST, type, tree);
    }
        
    return unary_expression();
}

static struct tree *
binary_expression(int prec)
{
    struct tree * left;
    struct tree * right;
    int           kk;

    if (prec < KK_PREC_MUL) return cast_expression();
    left = binary_expression(prec - 1);

    while (token.kk & KK_PREC(prec)) {
        kk = token.kk;
        lex();
        right = binary_expression(prec - 1);
        left = token_tree(kk, left, right, NULL);
    }

    return left;
}

struct tree *
conditional_expression(void)
{
    struct tree * condition;
    struct tree * left;
    struct tree * right;

    condition = binary_expression(KK_PREC_LOR);
    
    if (token.kk == KK_QUEST) {
        lex();
        left = expression();
        match(KK_COLON);
        right = conditional_expression();
        condition = scalar_expression(condition);
        left = void_pointer(left, right->type, VOID_POINTER_VOID);
        right = void_pointer(right, left->type, VOID_POINTER_VOID);
        condition = token_tree(KK_QUEST, left, right, condition);
        merge_qualifiers(condition->type, left->type);
        merge_qualifiers(condition->type, right->type);
    }

    return condition;
}

/* during normal parsing, 'left' is passed in NULL. 
   fake_assignment() calls this function with the left 
   side supplied, to "fake" an assignment. 

   if mode is ASSIGNMENT_CONST, then the 'const'ness
   of the left side will be ignored. */

struct tree *
assignment_expression(struct tree * left, int mode)
{
    struct tree * right;
    int           op;

    if (left)
        op = E_ASSIGN;
    else {
        left = conditional_expression();
        
        switch (token.kk) {
        case KK_EQ:         op = E_ASSIGN; break;
        case KK_PLUSEQ:     op = E_ADD; break;
        case KK_MINUSEQ:    op = E_SUB; break;
        case KK_STAREQ:     op = E_MUL; break;
        case KK_DIVEQ:      op = E_DIV; break;
        case KK_MODEQ:      op = E_MOD; break;
        case KK_SHLEQ:      op = E_SHL; break;
        case KK_SHREQ:      op = E_SHR; break;
        case KK_ANDEQ:      op = E_AND; break;
        case KK_BAREQ:      op = E_OR; break;
        case KK_XOREQ:      op = E_XOR; break;

        default:
            return left;
        }

        lex();
    }

    lvalue(left);

    if ((mode != ASSIGNMENT_CONST) && !modifiable(left->type)) 
        error(ERROR_ASSCONST);

    right = assignment_expression(NULL, 0);

    if (op == E_ASSIGN) {
        /* other assignment operators have limited combinations 
           of operands that make these unnecessary at best */
        right = void_pointer(right, left->type, VOID_POINTER_VOID);
        right = void_pointer(right, left->type, VOID_POINTER_OTHER);
        right = null_pointer(right, left->type);
        right = promote(right);
    }

    check_operand_types(op, left->type, right->type);

    /* this prevents ptr += and ptr -= from having their
       right-side integers cast. the only other situation
       with a left-side pointer type here is straight 
       assignment, which doesn't involve an implicit cast */

    if (!(left->type->ts & T_PTR) && !(left->type->ts & T_TAG)) {
        right = new_tree(E_CAST, copy_type(left->type), right);
        right->type->ts &= T_BASE; /* no bitfield here */
    }

    left = new_tree(op, copy_type(left->type), left, right);
    left = scale_pointers(left);
    if (op != E_ASSIGN) left->op = E_TO_ASSIGN(op);

    return left;
}

/* fake an assignment to a fake symbol of the right type, then discard
   everything but the right side of the assignment expression. this is
   done to get the right side type-checked and appropriately cast without
   actually doing an assignment; static initializer expressions and 
   prototyped function arguments use this. */

struct tree *
fake_assignment(struct type * type)
{
    struct symbol * symbol;
    struct tree   * tree;

    symbol = new_symbol(NULL, S_AUTO, copy_type(type));
    tree = symbol_tree(symbol);
    tree = assignment_expression(tree, ASSIGNMENT_CONST);
    decap_tree(tree, NULL, NULL, &tree, NULL);
    tree = generate(tree, GOAL_VALUE, NULL);
    free_symbol(symbol);

    return tree;
}

struct tree *
expression(void)
{
    struct tree * left;
    struct tree * right;

    left = assignment_expression(NULL, 0);

    while (token.kk == KK_COMMA) {
        lex();
        right = assignment_expression(NULL, 0);
        left = new_tree(E_COMMA, copy_type(right->type), right, left);
    }

    return left;
}

#ifndef NDEBUG

static char *trees[] = { 
    /*  0 */    "NOP", "SYM", "CON", "IMM", "MEM",
    /*  5 */    "REG", "FETCH", "ADDR", "NEG", "COM",
    /* 10 */    "CAST", "NOT", "ADD", "SUB", "MUL", 
    /* 15 */    "DIV", "MOD", "SHL", "SHR", "AND",  
    /* 20 */    "OR", "XOR", "ANDASS", "SUBASS", "MULASS", 
    /* 25 */    "DIVASS", "MODASS", "SHLASS", "SHRASS", "ANDASS",
    /* 30 */    "ORASS", "XORASS", "LAND", "LOR", "EQ", 
    /* 35 */    "NEQ", "GT", "LT", "GTEQ", "LTEQ", 
    /* 40 */    "COMMA", "ASSIGN", "PRE", "POST", "CALL", 
    /* 45 */    "TERN"
};

void
debug_tree(struct tree * tree)
{
    int i;

    if (tree) {
        fputc('(', stderr);
        debug_type(tree->type);
        fprintf(stderr, ": %s", trees[tree->op]);

        switch (tree->op) {
        case E_SYM:
            if (tree->u.sym->id) 
                fprintf(stderr, " '%s'", tree->u.sym->id->data);
            else
                fprintf(stderr, " @ %p", tree->u.sym);

            break;

        case E_CON:
            if (tree->type->ts & T_IS_INTEGRAL)
                fprintf(stderr, "=0x%lX", tree->u.con.i);
            else
                fprintf(stderr, "=%f", tree->u.con.f);

            break;

        case E_REG:
            fprintf(stderr, " %c%d", (tree->u.reg & R_IS_INTEGRAL) ? 'i' : 'f', R_IDX(tree->u.reg));
            break;

        case E_MEM:
        case E_IMM:
            if (tree->u.mi.rip) fprintf(stderr, " rip");
            if (tree->u.mi.b != R_NONE) fprintf(stderr, " b=i%d", R_IDX(tree->u.mi.b));
            if (tree->u.mi.i != R_NONE) fprintf(stderr, " i=i%d", R_IDX(tree->u.mi.i));
            if (tree->u.mi.s != 1) fprintf(stderr, " s=%d", R_IDX(tree->u.mi.s));

            if (tree->u.mi.glob) {
                fprintf(stderr, " glob=");
                if (tree->u.mi.glob->id) 
                    fprintf(stderr, "'%s'", tree->u.mi.glob->id->data);
                else
                    fprintf(stderr, "[%d]", tree->u.mi.glob->i);
            }

            if (tree->u.mi.ofs) fprintf(stderr, " %+ld", tree->u.mi.ofs);
            break;

        default:
            for (i = 0; i < NR_TREE_CH; i++) 
                debug_tree(tree->u.ch[i]);
        }
        
        fputc(')', stderr);

        if (tree->list) {
            fprintf(stderr, "..");
            debug_tree(tree->list);
        }
    }
}

#endif
