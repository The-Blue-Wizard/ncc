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

static void declarations(int (*)(int, struct string *, struct type *, void *, int), int, void *);

/* parse type qualifiers, updating the ts bitset. */

static void
qualifiers(int * ts)
{
    int t;

    for (;;)
    {
        t = 0;

        switch (token.kk)
        {
        case KK_CONST:      t = T_CONST; break;
        case KK_VOLATILE:   t = T_VOLATILE; break;
        }

        if (t) {
            if (*ts & t) error(ERROR_DUPQUAL);
            *ts |= t;
            lex();
        } else
            break;
    }
}

/* read a storage class specifier, if any, and return its
   mapped value */

static int
storage_class(void)
{
    int ss = S_NONE;

    switch (token.kk) {
    case KK_AUTO:       ss = S_AUTO; break;
    case KK_REGISTER:   ss = S_REGISTER; break;
    case KK_EXTERN:     ss = S_EXTERN; break;
    case KK_STATIC:     ss = S_STATIC; break;
    case KK_TYPEDEF:    ss = S_TYPEDEF; break;
    }

    if (ss) lex();
    return ss;
}

/* handle struct/union type specifiers. note that, unlike K&R, 
   struct tags are distinct from union tags, and each struct/union
   constitutes an independent namespace for its members. */

static int
declare_member(int ss, struct string * id, struct type * type, void * data, int first)
{
    int             align_bytes = align_of(type);
    int             type_bits = size_of(type) * BITS;
    long            offset_bits; /* long to catch overflows */
    struct symbol * member;
    int             member_bits;
    struct symbol * tag = data;

    if (ss) error(ERROR_SCLASS);

    if (tag->ss & S_UNION)
        offset_bits = 0;
    else
        offset_bits = tag->i;

    tag->align = MAX(tag->align, align_bytes);
    if (id && find_symbol_list(id, &(tag->list))) error(ERROR_REMEMBER);

    if (type->ts & T_FIELD) {
        member_bits = T_GET_SIZE(type->ts);

        if (    (member_bits == 0) 
                || (ROUND_DOWN(offset_bits, type_bits) 
                    != ROUND_DOWN(offset_bits + member_bits - 1, type_bits)))
        {
            offset_bits = ROUND_UP(offset_bits, type_bits);
        }
            
        T_SET_SHIFT(type->ts, offset_bits % type_bits);
    } else {
        offset_bits = ROUND_UP(offset_bits, align_bytes * BITS);
        member_bits = type_bits;
    }

    if (id) {
        member = new_symbol(id, S_MEMBER, type);
        member->i = ROUND_DOWN(offset_bits / BITS, align_bytes);
        put_symbol(member, current_scope);
        put_symbol_list(member, &(tag->list));
    } else
        free_type(type);

    if (tag->ss & S_STRUCT) {
        offset_bits += member_bits;
        if (offset_bits > MAX_SIZE) error(ERROR_TYPESIZE);
        tag->i = offset_bits;
    } else
        tag->i = MAX(tag->i, member_bits);

    return 0;
}

static struct type *
struct_or_union(void)
{
    struct symbol * tag;
    int             ss;
    struct string * id;
    struct type *   type;

    ss = (token.kk == KK_UNION) ? S_UNION : S_STRUCT;
    lex();

    if (token.kk == KK_IDENT) {
        id = token.u.text;
        lex();
        if ((token.kk == KK_LBRACE) || (token.kk == KK_SEMI))
            tag = find_symbol(id, S_TAG, current_scope, current_scope);
        else
            tag = find_symbol(id, S_TAG, SCOPE_GLOBAL, current_scope);
    } else {
        id = NULL;
        tag = NULL;
        expect(KK_LBRACE);
    }

    if (!tag) {
        tag = new_symbol(id, ss, NULL);
        put_symbol(tag, current_scope);
    }

    if (!(tag->ss & ss)) error(ERROR_TAGMATCH);

    if (token.kk == KK_LBRACE) {
        lex();
        if (tag->ss & S_DEFINED) error(ERROR_TAGREDEF);
        declarations(declare_member, DECLARATIONS_FIELDS, tag);
        match(KK_RBRACE);
        if (tag->i == 0) error(ERROR_EMPTY);
        tag->i = ROUND_UP(tag->i, tag->align * BITS);
        tag->i /= BITS;
        tag->ss |= S_DEFINED;
    }
    
    type = new_type(T_TAG);
    type->tag = tag;
    return type;
}

/* parse an enum specifier. */

static struct type *
enum_specifier(void)
{
    struct string * id;
    struct symbol * symbol;
    int             value;

    lex();

    if (token.kk == KK_IDENT) {
        id = token.u.text;
        lex();

        if (token.kk == KK_LBRACE) {
            if (find_symbol(id, S_TAG, current_scope, current_scope))
                error(ERROR_TAGREDEF);

            symbol = new_symbol(id, S_ENUM | S_DEFINED, NULL);
            put_symbol(symbol, current_scope);
        } else {
            symbol = find_symbol(id, S_TAG, SCOPE_GLOBAL, current_scope);
            if (!symbol) error(ERROR_UNKNOWN);
            if (!(symbol->ss & S_ENUM)) error(ERROR_TAGMATCH);
        }
    } else
        expect(KK_LBRACE);

    if (token.kk == KK_LBRACE) {
        lex();
        value = 0;

        for (;;) 
        {
            expect(KK_IDENT);
            id = token.u.text;
            lex();

            if (find_symbol(id, S_NORMAL, current_scope, current_scope)) error(ERROR_REDECL);
            symbol = new_symbol(id, S_CONST, NULL);

            if (token.kk == KK_EQ) {
                lex();
                value = constant_expression();
            }

            symbol->i = value++;
            put_symbol(symbol, current_scope);

            if (token.kk == KK_RBRACE)
                break;
            else 
                match(KK_COMMA);
        }

        match(KK_RBRACE);
    }

    return new_type(T_INT);
}

/* map a KK_TS_* bitset to a type, or error out. */

static struct
{
    int kk_ts;
    int t;
} type_map[] = {
    { KK_TS_VOID,                                   T_VOID },
    { KK_TS_INT,                                    T_INT },
    { KK_TS_SIGNED,                                 T_INT },
    { KK_TS_SIGNED | KK_TS_INT,                     T_INT },
    { KK_TS_UNSIGNED,                               T_UINT },
    { KK_TS_UNSIGNED | KK_TS_INT,                   T_UINT },
    { KK_TS_UNSIGNED | KK_TS_CHAR,                  T_UCHAR },
    { KK_TS_CHAR,                                   T_CHAR },
    { KK_TS_SIGNED | KK_TS_CHAR,                    T_SCHAR },
    { KK_TS_UNSIGNED | KK_TS_CHAR,                  T_UCHAR },
    { KK_TS_SHORT,                                  T_SHORT },
    { KK_TS_SIGNED | KK_TS_SHORT,                   T_SHORT },
    { KK_TS_SHORT | KK_TS_INT,                      T_SHORT },
    { KK_TS_SIGNED | KK_TS_SHORT | KK_TS_INT,       T_SHORT },
    { KK_TS_UNSIGNED | KK_TS_SHORT,                 T_USHORT },
    { KK_TS_UNSIGNED | KK_TS_SHORT | KK_TS_INT,     T_USHORT },
    { KK_TS_LONG,                                   T_LONG },
    { KK_TS_LONG | KK_TS_INT,                       T_LONG },
    { KK_TS_SIGNED | KK_TS_LONG,                    T_LONG },
    { KK_TS_SIGNED | KK_TS_LONG | KK_TS_INT,        T_LONG },
    { KK_TS_UNSIGNED | KK_TS_LONG,                  T_ULONG },
    { KK_TS_UNSIGNED | KK_TS_LONG | KK_TS_INT,      T_ULONG },
    { KK_TS_FLOAT,                                  T_FLOAT },
    { KK_TS_DOUBLE,                                 T_DOUBLE },
    { KK_TS_LONG | KK_TS_DOUBLE,                    T_LDOUBLE }
};

#define NR_TYPE_MAP (sizeof(type_map)/sizeof(*type_map))

static struct type * 
map_type(int kk_ts)
{
    int i;

    for (i = 0; i < NR_TYPE_MAP; ++i) 
        if (type_map[i].kk_ts == kk_ts)
            return new_type(type_map[i].t);

    error(ERROR_ILLTYPE);
}

/* parse type specifier. if 'ss' is not NULL, then also allow
   a storage-class specifier. if no type specifiers at all 
   are encountered, return NULL. */

static struct type *
type_specifier(int * ss)
{
    struct symbol * symbol;
    struct type   * type = NULL;
    int             kk_ts = 0;
    int             qual_ts = 0;
    struct type   * tmp;

    if (ss) *ss = S_NONE;

    for (;;)
    {
        switch (token.kk)
        {
        case KK_AUTO:
        case KK_REGISTER:
        case KK_EXTERN:
        case KK_STATIC:
        case KK_TYPEDEF:
            if (!ss) error(ERROR_SCLASS);
            if (*ss != S_NONE) error(ERROR_DUPCLASS);
            *ss = storage_class();
            break;

        case KK_CONST:
        case KK_VOLATILE:
            qualifiers(&qual_ts);
            break;

        case KK_STRUCT:
        case KK_UNION:
        case KK_ENUM:
            if (type) error(ERROR_ILLTYPE);
            type = (token.kk == KK_ENUM) ? enum_specifier() : struct_or_union();
            break;

        case KK_SIGNED:
        case KK_VOID:
        case KK_UNSIGNED:
        case KK_SHORT:
        case KK_LONG:
        case KK_INT: 
        case KK_FLOAT: 
        case KK_DOUBLE:
        case KK_CHAR: 
            if ((token.kk & KK_TS_MASK) & kk_ts) error(ERROR_DUPSPEC);
            kk_ts |= token.kk & KK_TS_MASK;
            lex();
            break;

        case KK_IDENT:
            if (!type) {
                symbol = find_typedef(token.u.text);

                if (symbol) {
                    lex();
                    type = copy_type(symbol->type);
                    break;
                }
            }
            /* otherwise fall through */
        default:
            if (kk_ts && type) error(ERROR_ILLTYPE);
            if (kk_ts) type = map_type(kk_ts);

            if (!type) {
                if (!(ss && *ss) && !qual_ts) 
                    return NULL;
                else
                    type = new_type(T_INT);
            }

            if (qual_ts) {  /* apply to array elements */
                tmp = type;
                while (tmp->ts & T_ARRAY) tmp = tmp->next;
                tmp->ts |= qual_ts;
            }

            return type;
        }
    }
}

/* these three functions parse a declarator - declarator() is the entry.

   '*id' will be set to the identifier declared, unless 'id' is NULL, in
   which case an identifier will be prohibited (for abstract declarators).

   if 'old_args' is not NULL, then old-style arguments will be permitted. they are
   placed in the symbol table as S_OLD_ARG at SCOPE_FUNCTION, linked together in a 
   list. '*old_args' will point to the first symbol of that list. this scheme is 
   fragile and an abuse of the symbol table, but it works because:

   1. old-style arguments are only allowed in definitions, at SCOPE_GLOBAL, 
   2. the caller will immediately enter SCOPE_FUNCTION after the declarator is read. */

static struct type * direct_declarator(struct string **, struct symbol **);

static struct type *
declarator(struct string ** id, struct symbol ** old_args)
{
    struct type * type;
    int           t_quals = 0;

    if (token.kk == KK_STAR) {
        lex();
        qualifiers(&t_quals);
        type = splice_types(declarator(id, old_args), new_type(T_PTR | t_quals));
    } else
        type = direct_declarator(id, old_args);

    return type;
}

/* process a prototype argument list. returns a T_FUNC type node
   with the 'proto' data filled in appropriately. */

static struct type *
proto_arguments(void)
{
    struct symbol * symbol;
    struct proto  * proto;
    struct type   * type;
    struct string  * id;
    int             ss;

    proto = new_proto();

    match(KK_LPAREN);
    enter_scope();

    for (;;) {
        if (token.kk == KK_ELLIP) {
            lex();
            proto->ps |= P_VARIADIC;

            if (!proto->args) 
                error(ERROR_VARIADIC);
            else
                break;
        }

        id = NULL;
        type = type_specifier(&ss);

        switch (ss) {
        case S_NONE:
            ss = S_LOCAL;
            break;

        case S_AUTO:
        case S_REGISTER:
            break;

        default:
            error(ERROR_SCLASS);
        }

        type = splice_types(declarator(&id, NULL), type);
        validate_type(type);
        type = argument_type(type, ARGUMENT_TYPE_NEW);

        if (type->ts & T_VOID) 
            if ((proto->args) || id) 
                error(ERROR_ILLVOID);
            else
                break;

        if (id) {
            symbol = find_symbol_list(id, &proto->args);
            if (symbol) error(ERROR_DUPARG);
        }

        symbol = new_symbol(id, ss, type);
        put_symbol_list(symbol, &proto->args);

        if (token.kk == KK_COMMA) 
            lex();
        else
            break;
    }

    match(KK_RPAREN);
    exit_scope(EXIT_SCOPE_PROTO);

    type = new_type(T_FUNC);
    type->proto = proto;
    return type;
}

/* process an old-style function declaration. add
   args to list headed by 'old_args', if present */

static struct type *
old_arguments(struct symbol ** old_args)
{
    struct symbol * symbol;

    match(KK_LPAREN);

    if (token.kk == KK_IDENT) {
        if (!old_args) error(ERROR_NOARGS);

        for (;;) {
            expect(KK_IDENT);
            symbol = find_symbol(token.u.text, S_OLD_ARG, SCOPE_FUNCTION, SCOPE_FUNCTION);
            if (symbol) error(ERROR_DUPARG);
            symbol = new_symbol(token.u.text, S_OLD_ARG, NULL);
            put_symbol(symbol, SCOPE_FUNCTION);
            put_symbol_list(symbol, old_args);
            lex();

            if (token.kk == KK_COMMA)
                lex();
            else
                break;
        }
    }

    match(KK_RPAREN);
    return new_type(T_FUNC);
}

static struct type *
direct_declarator(struct string ** id, struct symbol ** old_args)
{
    struct type * type = NULL;
    struct type * array_type;

    if (token.kk == KK_LPAREN) {
        if ((peek(NULL) != KK_RPAREN) && !peek_type_specifier()) {
            lex();
            type = declarator(id, old_args);
            match(KK_RPAREN);
        }
    } else if (token.kk == KK_IDENT) {
        if (!id) error(ERROR_ABSTRACT);
        *id = token.u.text;
        lex();
    } 

    for (;;) {
        if (token.kk == KK_LBRACK) {
            lex();
            array_type = new_type(T_ARRAY);
            if (token.kk != KK_RBRACK) {
                array_type->nr_elements = constant_expression();
                if (array_type->nr_elements == 0) error(ERROR_ILLARRAY);
            }
            match(KK_RBRACK);
            type = splice_types(type, array_type);
        } else if (token.kk == KK_LPAREN) {
            if (peek_type_specifier()) {
                /* prototype or new-style definition */
                type = splice_types(type, proto_arguments());
            } else {
                /* old-style declaration or definition */
                type = splice_types(type, old_arguments(type ? NULL : old_args));
            }
        } else break;
    }

    return type;
}

/* parse an abstract type and return the resultant type. */

struct type *
abstract_type(void)
{
    struct type * type;

    type = type_specifier(NULL);
    if (type == NULL) error(ERROR_ABSTRACT);
    type = splice_types(declarator(NULL, NULL), type);
    validate_type(type);
    return type;
}

/* process a set of declarations. after each declarator is read, the declare()
   function is invoked with as declare(ss, id, type, ptr, first) where:

   struct string * id   is the identifier declared,
   int ss               is the explicit storage class (or S_NONE),
   struct type * type   is the type declared (ownership given to declare()),
   first                is zero if this is not the first declarator 
   
   and 'ptr' is either struct symbol *, when mode is DECLARATIONS_ARGS,
   giving the head of the formal argument list in the symbol table. otherwise
   it's char * 'data', just copied from the call to declarations().

   the flags are:
   DECLARATIONS_ARGS    allow old-style arguments ( = allow function definitions)
   DECLARATIONS_INT     in the absence of any storage class or type specifier, assume 'int'
   DECLARATIONS_FIELDS  allow bit field types

   the first two are used only for external definitions (at file scope), and the last
   for struct/union member declarations. all other callers set flags to 0.
 */

static void
declarations(int (*declare)(int, struct string *, struct type *, void *, int), int flags, void * data)
{
    struct type *   base;
    struct type *   type;
    int             ss;
    struct string * id;
    struct symbol * old_args;
    int             first;
    int             bits;

    while (token.kk != KK_NONE) {
        first = 1;
        base = type_specifier(&ss);

        if (base == NULL) {
            if (flags & DECLARATIONS_INTS)
                base = new_type(T_INT);
            else
                break;
        }

        if (token.kk != KK_SEMI) {
            for (;;) {
                old_args = NULL;
                id = NULL;

                if (token.kk == KK_COLON) {
                    type = copy_type(base);
                } else {
                    type = declarator(&id, ((flags & DECLARATIONS_ARGS) && first) ? &old_args : NULL);
                    if (id == NULL) error(ERROR_MISSING);
                    type = splice_types(type, copy_type(base));
                }

                if (token.kk == KK_COLON) {
                    if (!(flags & DECLARATIONS_FIELDS)) error(ERROR_ILLFIELD);
                    if (!(type->ts & T_IS_INTEGRAL)) error(ERROR_FIELDTY);
                    lex();
                    bits = constant_expression();
                    if ((bits < 0) || (bits > (size_of(type) * BITS))) error(ERROR_FIELDSZ);
                    if ((bits == 0) && id) error(ERROR_FIELDSZ);
                    type->ts |= T_FIELD;
                    T_SET_SIZE(type->ts, bits);
                }

                validate_type(type);
                if (type->ts & T_VOID) error(ERROR_ILLVOID);

                if (declare(ss, id, type, (flags & DECLARATIONS_ARGS) ? (char *) old_args : data, first)) 
                    goto was_definition;

                first = 0;

                if (token.kk == KK_COMMA)
                    lex();
                else
                    break;
            }
        }

        match(KK_SEMI);

        was_definition:
        free_type(base);
    }
}

/* parse the top-of-block declarations that are local in scope. */

static int 
declare_local(int ss, struct string * id, struct type * type, void * data, int first)
{
    struct symbol * symbol;

    if (type->ts & T_FUNC) {
        if (!ss) ss = S_EXTERN;
        if (ss != S_EXTERN) error(ERROR_SCLASS);
    }

    if (ss & S_EXTERN) {
        symbol = find_symbol(id, S_EXTERN | S_STATIC | S_LURKER, SCOPE_GLOBAL, SCOPE_GLOBAL);

        if (symbol)
            compat_types(symbol->type, type, 0);
        else {
            symbol = new_symbol(id, S_EXTERN | S_LURKER, copy_type(type));
            put_symbol(symbol, SCOPE_GLOBAL);
        }

        if (symbol->ss & S_EXTERN) symbol->ss |= S_REFERENCED;
    } 

    if (ss == S_NONE) ss = S_LOCAL;
    symbol = find_symbol(id, S_NORMAL, current_scope, current_scope);
    if (symbol) error(ERROR_REDECL);
    symbol = new_symbol(id, ss, type);
    put_symbol(symbol, current_scope);
    initializer(symbol, ss);

    return 0;
}

/* process an implicit function declaration */

void
declare_implicit(struct string * id)
{
    declare_local(S_EXTERN, id, splice_types(new_type(T_FUNC), new_type(T_INT)), NULL, 0);
}

void
local_declarations(void)
{
    declarations(declare_local, 0, NULL);
}

/* process old-style function argument type declaration */

static int
declare_argument(int ss, struct string * id, struct type * type, void * data, int first)
{
    struct symbol * symbol;
    struct symbol * old_args = data;

    symbol = find_symbol_list(id, &old_args);
    if (symbol == NULL) error(ERROR_NOTARG);
    if (symbol->type) error(ERROR_REDECL);
    symbol->type = argument_type(type, ARGUMENT_TYPE_OLD);

    switch (ss) 
    {
    case S_AUTO:
    case S_REGISTER:  
        symbol->ss = ss;
        break;
    case S_NONE:
        symbol->ss = S_LOCAL;
        break;
    default:
        error(ERROR_SCLASS);
    }

    return 0;
}

/* parse a function definition. this actually encapsulates 
   and drives the entire code-generation process. */

static void
compute_offset(struct symbol * symbol)
{
    symbol->i = frame_offset;
    frame_offset += size_of(symbol->type);
    frame_offset = ROUND_UP(frame_offset, FRAME_ALIGN);
}

static void
function_definition(struct symbol * symbol, struct symbol * old_args)
{
    struct symbol * arg;

    if (symbol->ss & S_DEFINED) error(ERROR_DUPDEF);
    current_function = symbol;
    frame_offset = FRAME_ARGUMENTS;

    /* if the function returns a struct, the caller will pass in a
       pointer to the struct, and we stash it in an unnamed temporary. */

    if (current_function->type->next->ts & T_TAG) 
        return_struct_temp = temporary(splice_types(new_type(T_PTR), copy_type(current_function->type->next)));
    else
        return_struct_temp = NULL;

    /* parse types of old-style arguments already in SCOPE_FUNCTION,
       or import new-style arguments into SCOPE_FUNCTION, and compute
       their frame addresses */

    if (old_args || !current_function->type->proto || (current_function->type->proto->ps & P_STALE)) {
        declarations(declare_argument, 0, old_args);

        for (arg = old_args; arg; arg = arg->list) {
            if (arg->type == NULL) {
                arg->type = new_type(T_INT);
                arg->ss = S_LOCAL;
            }
            compute_offset(arg);
        }
    } else {
        for (arg = current_function->type->proto->args; arg; arg = arg->list) {
            if (arg->id == NULL) error(ERROR_ARGNAME);
            symbol = new_symbol(arg->id, arg->ss, copy_type(arg->type));
            compute_offset(symbol);
            put_symbol(symbol, SCOPE_FUNCTION);
        }
    }

    frame_offset = 0;
    setup_blocks();
    compound();         /* will enter_scope() to capture arguments at SCOPE_FUNCTION */
    optimize();
    output_function();
    free_blocks();
    free_symbols();
    current_function->ss |= S_DEFINED;
    current_function = NULL;
}

/* translation_unit() is the goal symbol of the parser. a C
   source file boils down to a list of external definitions. */

static int
declare_global(int ss, struct string * id, struct type * type, void * data, int first)
{
    struct symbol * symbol;
    int             effective_ss;
    struct symbol * old_args = data;

    if (ss & (S_AUTO | S_REGISTER)) error(ERROR_SCLASS);
    effective_ss = (ss == S_NONE) ? S_EXTERN : ss;

    symbol = find_symbol(id, S_NORMAL | S_LURKER, SCOPE_GLOBAL, SCOPE_GLOBAL);

    if (symbol) {
        if (    (symbol->ss & effective_ss & (S_EXTERN | S_STATIC))         /* = same storage class */
            ||  ((symbol->ss & S_STATIC) && (effective_ss & S_EXTERN)) )    /* static redeclared extern is OK */
        {
            compat_types(symbol->type, type, COMPAT_TYPES_COMPOSE | COMPAT_TYPES_QUALS);
            free_type(type);
        } else
            error(ERROR_REDECL);
    } else {
        symbol = new_symbol(id, effective_ss, type);
        put_symbol(symbol, SCOPE_GLOBAL);
    }

    symbol->ss &= ~S_LURKER;

    if (symbol->type->ts & T_FUNC) {
        if (old_args || ((token.kk != KK_COMMA) && (token.kk != KK_SEMI))) {
            if (!first) error(ERROR_SYNTAX);
            function_definition(symbol, old_args);
            return 1;
        }
    } else
        initializer(symbol, ss);

    return 0;
}

void
translation_unit(void)
{
    lex();  
    declarations(declare_global, DECLARATIONS_INTS | DECLARATIONS_ARGS, NULL);
    expect(KK_NONE);
}
