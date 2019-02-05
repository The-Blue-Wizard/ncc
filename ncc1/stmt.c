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
#include "ncc1.h"

/* switch cases are kept in a linked list, 
   ordered by 'value' (least to greatest) */

struct switch_case
{
    struct tree        * value;
    struct block       * target;
    struct switch_case * next;
};

/* these variables are associated with the closest-enclosing 
   block of appropriate type. the parser routines deal with 
   saving/restoring their contents in their activation records. */

static struct switch_case * switch_cases;
static int                  nr_switch_cases;
static struct type        * switch_type;
static struct block       * break_block;
static struct block       * continue_block;
static struct block       * default_block;

static void statement(void);

/* if-statement : IF '(' expression ')' statement */

static void
if_statement(void)
{
    struct block * true_block;
    struct block * else_block;
    struct block * join_block;
    struct tree  * test;
    int            cc;

    true_block = new_block();
    else_block = new_block();
    join_block = new_block();
    
    lex();
    match(KK_LPAREN);
    test = expression();
    test = scalar_expression(test);
    generate(test, GOAL_CC, &cc);
    succeed_block(current_block, cc, true_block);
    succeed_block(current_block, CC_INVERT(cc), else_block);
    match(KK_RPAREN);

    current_block = true_block;
    statement();
    true_block = current_block;
    succeed_block(true_block, CC_ALWAYS, join_block);

    if (token.kk == KK_ELSE) {
        lex();
        current_block = else_block;
        statement();
        else_block = current_block;
    }

    succeed_block(else_block, CC_ALWAYS, join_block);
    current_block = join_block;
}

/* while-statement : WHILE '(' expression ')' statement */

static void
while_statement(void)
{
    struct block * test_block;
    struct block * body_block;
    struct block * saved_break_block;
    struct block * saved_continue_block;
    struct tree *  test;
    int            cc;

    saved_continue_block = continue_block;
    saved_break_block = break_block;

    test_block = new_block();
    body_block = new_block();
    break_block = new_block();
    continue_block = test_block;
    succeed_block(current_block, CC_ALWAYS, test_block);
    current_block = test_block;

    lex();
    match(KK_LPAREN);
    test = expression();
    test = scalar_expression(test);
    generate(test, GOAL_CC, &cc);
    succeed_block(current_block, cc, body_block);
    succeed_block(current_block, CC_INVERT(cc), break_block);
    match(KK_RPAREN);

    current_block = body_block;
    statement();
    body_block = current_block;
    succeed_block(body_block, CC_ALWAYS, test_block);

    current_block = break_block;
    continue_block = saved_continue_block;
    break_block = saved_break_block;
}

/* do-statement : DO statement WHILE '(' expression ')' ';' */

static void
do_statement(void)
{
    struct block * saved_continue_block;
    struct block * saved_break_block;
    struct block * body_block;
    struct tree *  test;
    int            cc;

    saved_continue_block = continue_block;
    saved_break_block = break_block;
    continue_block = new_block();
    break_block = new_block();
    body_block = new_block();
    succeed_block(current_block, CC_ALWAYS, body_block);
    current_block = body_block;

    lex();
    statement();
    match(KK_WHILE);
    match(KK_LPAREN);
    test = expression();
    test = scalar_expression(test);
    succeed_block(current_block, CC_ALWAYS, continue_block);
    current_block = continue_block;
    generate(test, GOAL_CC, &cc);
    match(KK_RPAREN);
    match(KK_SEMI);
    succeed_block(current_block, cc, body_block);
    succeed_block(current_block, CC_INVERT(cc), break_block);

    current_block = break_block;
    continue_block = saved_continue_block;
    break_block = saved_break_block;
}

/* return-statement : RETURN [expression] ';' */

static void
return_statement(void)
{
    struct type * return_type;
    struct tree * tree;

    match(KK_RETURN);
    return_type = current_function->type->next;

    if (token.kk != KK_SEMI) {
        if (return_type->ts & T_VOID)
            error(ERROR_RETURN);

        if (return_type->ts & T_TAG) {
            tree = copy_tree(return_struct_temp);
            tree = new_tree(E_FETCH, copy_type(tree->type->next), tree);
        } else if (return_type->ts & T_IS_FLOAT) 
            tree = reg_tree(R_XMM0, copy_type(return_type));
        else
            tree = reg_tree(R_AX, copy_type(return_type));

        tree = assignment_expression(tree, ASSIGNMENT_CONST);
        generate(tree, GOAL_EFFECT, NULL);
    }

    succeed_block(current_block, CC_ALWAYS, exit_block);
    current_block = new_block();
    match(KK_SEMI);
}

/* for-statement : FOR '(' [expression] ';' [expression] ';' [expression] ')' statement */

static void
for_statement(void)
{
    struct block * saved_continue_block;
    struct block * saved_break_block;
    struct block * test_block;
    struct block * body_block;
    struct tree *  initial = NULL;
    struct tree *  test = NULL;
    struct tree *  step = NULL;
    int            cc;

    saved_continue_block = continue_block;
    saved_break_block = break_block;
    test_block = new_block();
    body_block = new_block();
    continue_block = new_block();
    break_block = new_block();

    lex();
    match(KK_LPAREN);
    if (token.kk != KK_SEMI) initial = expression();
    match(KK_SEMI);
    if (token.kk != KK_SEMI) test = expression();
    match(KK_SEMI);
    if (token.kk != KK_RPAREN) step = expression();
    match(KK_RPAREN);

    if (initial) generate(initial, GOAL_EFFECT, NULL);
    succeed_block(current_block, CC_ALWAYS, test_block);
    current_block = test_block;

    if (test) {
        generate(test, GOAL_CC, &cc);
        succeed_block(current_block, cc, body_block);
        succeed_block(current_block, CC_INVERT(cc), break_block);
    } else
        succeed_block(current_block, CC_ALWAYS, body_block);

    current_block = body_block;
    statement();
    succeed_block(current_block, CC_ALWAYS, continue_block);

    current_block = continue_block;
    if (step) generate(step, GOAL_EFFECT, NULL);
    succeed_block(current_block, CC_ALWAYS, test_block);
    
    current_block = break_block;
    continue_block = saved_continue_block;
    break_block = saved_break_block;
}

void
compound(void)
{
    enter_scope();
    match(KK_LBRACE);

    /* if we're entering function scope and the function returns
       a struct/union, we need to save the return-struct pointer
       (which is passed by the caller in RAX) in the temporary
       allocated for that purpose before doing anything else. */

    if ((current_scope == SCOPE_FUNCTION) && return_struct_temp) 
        choose(E_ASSIGN, copy_tree(return_struct_temp), reg_tree(R_AX, new_type(T_LONG)));

    local_declarations();
    while (token.kk != KK_RBRACE) statement();
    match(KK_RBRACE);
    exit_scope(EXIT_SCOPE_BLOCK);
}

/* label-statement : IDENTIFIER ':' statement */

static void
label_statement(void)
{
    struct symbol * label;

    label = find_label(token.u.text);
    if (label->ss & S_DEFINED) error(ERROR_DUPDEF);
    label->ss |= S_DEFINED;

    lex();
    match(KK_COLON);

    succeed_block(current_block, CC_ALWAYS, label->target);
    current_block = label->target;

    statement();
}

/* goto-statement: GOTO IDENTIFIER ';' */

static void
goto_statement(void)
{
    struct symbol * label;

    match(KK_GOTO);
    expect(KK_IDENT);
    label = find_label(token.u.text);
    match(KK_IDENT);
    match(KK_SEMI);

    succeed_block(current_block, CC_ALWAYS, label->target);
    current_block = new_block();
}

static void
switch_search(struct tree * reg, struct switch_case * first, int count)
{
    struct switch_case * switch_case = first;
    int                  lower_count = 0;
    int                  higher_count = count - 1;
    struct block       * lower_block;
    struct block       * higher_block;
    struct block       * temp_block;

    if (count == 0) 
        succeed_block(current_block, CC_ALWAYS, default_block);
    else {
        while ((lower_count < higher_count) && switch_case->next) {
            switch_case = switch_case->next;
            ++lower_count;
            --higher_count;
        }

        temp_block = current_block;

        lower_block = new_block();
        current_block = lower_block;
        switch_search(reg, first, lower_count);

        higher_block = new_block();
        current_block = higher_block;
        switch_search(reg, switch_case->next, higher_count);

        current_block = temp_block;
        choose(E_EQ, copy_tree(reg), copy_tree(switch_case->value));
        succeed_block(current_block, CC_Z, switch_case->target);

        if (lower_count || higher_count) {
            succeed_block(current_block, CC_L, lower_block);
            succeed_block(current_block, CC_G, higher_block);
        } else 
            succeed_block(current_block, CC_NZ, default_block);
    }
}

/* switch-statement: SWITCH '(' expression ')' statement */

static void
switch_statement(void)
{
    struct switch_case * saved_switch_cases;
    int                  saved_nr_switch_cases;
    struct block       * saved_default_block;
    struct block       * saved_break_block;
    struct type        * saved_switch_type;
    struct tree        * tree;
    struct tree        * reg_ax;
    struct block       * control_block;
    struct switch_case * switch_case;
    
    saved_switch_cases = switch_cases;
    saved_nr_switch_cases = nr_switch_cases;
    saved_default_block = default_block;
    saved_break_block = break_block;
    saved_switch_type = switch_type;
    switch_cases = NULL;
    nr_switch_cases = 0;
    default_block = NULL;
    break_block = new_block();
    lex();
    match(KK_LPAREN);
    tree = expression();
    tree = promote(tree);
    tree = generate(tree, GOAL_VALUE, NULL);
    if (!(tree->type->ts & T_IS_INTEGRAL)) error(ERROR_CASE);
    switch_type = copy_type(tree->type);
    reg_ax = reg_tree(R_AX, copy_type(switch_type));
    choose(E_ASSIGN, copy_tree(reg_ax), tree);
    match(KK_RPAREN);
    control_block = current_block;
    current_block = new_block();
    statement();
    succeed_block(current_block, CC_ALWAYS, break_block);
    if (default_block == NULL) default_block = break_block;
    current_block = control_block;
    switch_search(reg_ax, switch_cases, nr_switch_cases);

    while (switch_case = switch_cases) {
        switch_cases = switch_cases->next;
        free_tree(switch_case->value);
        free(switch_case);
    }

    free_tree(reg_ax);
    free_type(switch_type);
    current_block = break_block;
    switch_cases = saved_switch_cases;
    nr_switch_cases = saved_nr_switch_cases;
    default_block = saved_default_block;
    break_block = saved_break_block;
    switch_type = saved_switch_type;
}

/* case-statement : CASE constant-expression ':' statement 
                  | DEFAULT ':' statement */

static void
case_statement(void)
{
    struct switch_case ** switch_casep;
    struct switch_case  * switch_case;
    struct tree         * value;

    if (switch_type == NULL) error(ERROR_MISPLACED);

    if (token.kk == KK_DEFAULT) {
        lex();
        if (default_block) error(ERROR_DUPCASE);
        default_block = new_block();
        succeed_block(current_block, CC_ALWAYS, default_block);
        current_block = default_block;
    } else {
        lex();

        value = expression();
        if (!(value->type->ts & T_IS_INTEGRAL)) error(ERROR_CASE);
        value = new_tree(E_CAST, copy_type(switch_type), value);
        value = generate(value, GOAL_VALUE, NULL);
        if (value->op != E_CON) error(ERROR_CONEXPR);

        for (switch_casep = &switch_cases; *switch_casep; switch_casep = &((*switch_casep)->next)) {
            if ((*switch_casep)->value->u.con.i == value->u.con.i) error(ERROR_DUPCASE); 
            if ((*switch_casep)->value->u.con.i > value->u.con.i) break;
        }

        switch_case = allocate(sizeof(struct switch_case));
        switch_case->value = value;
        switch_case->target = new_block();
        switch_case->next = *switch_casep;
        *switch_casep = switch_case;
        ++nr_switch_cases;
        succeed_block(current_block, CC_ALWAYS, switch_case->target);
        current_block = switch_case->target;
    }
    
    match(KK_COLON);
    statement();
}

/* statement : ';'
             | compound-statement
             | BREAK ';'
             | CONTINUE ';'
             | case-statement
             | do-statement
             | if-statement
             | for-statement
             | goto-statement
             | switch-statement
             | while-statement
             | return-statement
             | label-statement 
             | expression ';' */

static void
loop_control(struct block * block)
{
    lex();
    if (block == NULL) error(ERROR_MISPLACED);
    succeed_block(current_block, CC_ALWAYS, block);
    current_block = new_block();
    match(KK_SEMI);
}

static void
statement(void)
{
    struct tree * tree;

    switch (token.kk) {
    case KK_LBRACE:
        compound();
        break;
    case KK_BREAK:
        loop_control(break_block);
        break;
    case KK_CONTINUE:
        loop_control(continue_block);
        break;
    case KK_CASE:
    case KK_DEFAULT:
        case_statement();
        break;
    case KK_DO:
        do_statement();
        break;
    case KK_IF:
        if_statement();
        break;
    case KK_FOR:
        for_statement();
        break;
    case KK_GOTO:
        goto_statement();
        break;
    case KK_SWITCH:
        switch_statement();
        break;
    case KK_WHILE:
        while_statement();
        break;
    case KK_RETURN:
        return_statement();
        break;
    case KK_IDENT:
        if (peek(NULL) == KK_COLON) {
            label_statement();
            break;
        }
        /* fall through */
    default: 
        tree = expression();
        tree = generate(tree, GOAL_EFFECT, NULL);
    case KK_SEMI:
        match(KK_SEMI);
    }
}

