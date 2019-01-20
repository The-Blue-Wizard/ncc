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

/* create a new type node and give it sane defaults */

struct type *
new_type(int ts)
{
    struct type * type;

    type = (struct type *) allocate(sizeof(struct type));
    type->ts = ts;
    type->nr_elements = 0;
    type->tag = NULL;
    type->proto = NULL;
    type->next = NULL;
    return type;
}

/* allocate a new proto for a new-style function. 
   it's returned with a reference count of 1, so it
   should be immediately assigned to its type node. */

struct proto *
new_proto(void)
{
    struct proto * proto;

    proto = (struct proto *) allocate(sizeof(struct proto));
    proto->ps = 0;
    proto->refs = 1;
    proto->args = NULL;

    return proto;
}

/* "copy" a proto, i.e., bump its ref count */

struct proto *
copy_proto(struct proto * proto)
{
    ++proto->refs;
    return proto;
}

/* "free" a proto, i.e., decrement its ref count and 
   free its resources if it's fallen to zero */

void
free_proto(struct proto * proto)
{
    if (--proto->refs == 0) {
        free_symbol_list(&proto->args);
        free(proto);
    }
}

/* free a type - the whole type, not just the node.
   safe to call with NULL 'type'. */

void
free_type(struct type * type)
{
    struct type * tmp;

    while (type) {
        if (type->proto) free_proto(type->proto);
        tmp = type->next;
        free(type);
        type = tmp;
    }
}

/* return a copy of the type */

struct type *
copy_type(struct type * type)
{
    struct type *  copy = NULL;
    struct type ** typep = &copy;

    while (type) {
        *typep = new_type(type->ts);
        (*typep)->nr_elements = type->nr_elements;
        (*typep)->tag = type->tag;
        if (type->proto) (*typep)->proto = copy_proto(type->proto);
        type = type->next;
        typep = &((*typep)->next);
    }

    return copy;
}

/* glue two types into one by appending 'type2' on the end of 'type1'. */

struct type *
splice_types(struct type * type1, struct type * type2)
{
    struct type * tmp;

    if (type1 == NULL) return type2;
    for (tmp = type1; tmp->next; tmp = tmp->next) ;
    tmp->next = type2;

    return type1;
}

/* check for compatibility between two types.

   if COMPAT_TYPES_COMPOSE is specified, additional type
   information from 'type2' is merged into 'type1'.

   if COMPAT_TYPES_QUALS is specified, the types must carry
   exactly the same qualifiers. 

   if COMPAT_TYPES_ASSIGN is specified, then if 'type1' and
   'type2' are pointer types, then the type pointed to by 
   'type1' must be at least as qualified as the type pointed
   to by 'type2'. */

void
compat_types(struct type * type1, struct type * type2, int flags)
{
    struct symbol * arg1;
    struct symbol * arg2;
    int             assign = 0; 
    int             disqualified = 0;

    while (type1 && type2) {
        if ((type1->ts & T_BASE) != (type2->ts & T_BASE)) break;
        if (type1->tag != type2->tag) break;
        if ((type1->nr_elements && type2->nr_elements) && (type1->nr_elements != type2->nr_elements)) break;

        if (flags & COMPAT_TYPES_COMPOSE) {
            if (type2->nr_elements) type1->nr_elements = type2->nr_elements;

            if (type2->proto) {
                if (type1->proto) {
                    if ((type1->proto->ps & P_VARIADIC) != (type2->proto->ps & P_VARIADIC))
                        error(ERROR_INCOMPAT);
                    
                    arg1 = type1->proto->args;
                    arg2 = type2->proto->args;

                    while (arg1 && arg2) {
                        arg1->id = arg2->id;
                        if (arg1->ss != arg2->ss) error(ERROR_INCOMPAT);
                        compat_types(arg1->type, arg2->type, COMPAT_TYPES_COMPOSE | COMPAT_TYPES_QUALS);
                        arg1 = arg1->list;
                        arg2 = arg2->list;
                    }

                    if (arg1 || arg2) error(ERROR_INCOMPAT);
                    type1->proto->ps &= ~P_STALE;
                } else 
                    type1->proto = copy_proto(type2->proto);
            } else if (type1->proto) {
                type1->proto->ps |= P_STALE;
            }
        }

        if ((flags & COMPAT_TYPES_QUALS) && ((type1->ts & T_QUAL_MASK) != (type2->ts & T_QUAL_MASK))) break;

        /* order is important here, as we don't want to check assignment
           qualifiers in the first pass, and we don't bother unless the
           top levels are pointers. */

        if ((assign == 1) && ((type1->ts & type2->ts & T_QUAL_MASK) != (type2->ts & T_QUAL_MASK)))
            ++disqualified;

        if ((flags & COMPAT_TYPES_ASSIGN) && !assign)
            if (type1->ts & type2->ts & T_PTR)
                ++assign;   /* 1 = check assignment qualifiers */
            else
                --assign;   /* -1 = don't check */

        type1 = type1->next;
        type2 = type2->next;
    }

    if (type1 || type2) error(ERROR_INCOMPAT);
    if (disqualified) error(ERROR_DISQUAL);
}

/* merge type qualifiers from 'dst' into 'src'.
   caller must ensure that the types are compatible. */

void
merge_qualifiers(struct type * dst, struct type * src)
{
    while (dst) 
    {
        dst->ts |= src->ts & T_QUAL_MASK;
        dst = dst->next;
        src = src->next;
    }
}

/* returns non-zero if the type is modifiable. must recurse
   through struct elements, since they can be 'const'. */

int
modifiable(struct type * type)
{
    struct symbol * symbol;

    if (type->ts & T_CONST) return 0;

    if (type->ts & T_TAG) {
        for (symbol = type->tag->list; symbol; symbol = symbol->list) 
            if (!modifiable(symbol->type)) return 0;
    }

    return 1;
}

/* return true if the type is complete, false otherwise */

int
complete_type(struct type * type)
{
    while (type) {
        if ((type->ts & T_ARRAY) && (type->nr_elements == 0))
            return 0;

        if ((type->ts & T_TAG) && !(type->tag->ss & S_DEFINED))
            return 0;

        type = type->next;
    }

    return 1;
}

/* return the size or alignment of the type in bytes.
   abort with an error if the value isn't, or can't be, known. */

int
size_of(struct type * type)
{
    long size = 1;

    while (type) {
        if (type->ts & T_IS_BYTE) 
            /* size *= 1 */ ;
        else if (type->ts & T_IS_WORD) 
            size *= 2;
        else if (type->ts & T_IS_DWORD) 
            size *= 4;
        else if (type->ts & T_IS_QWORD)
            size *= 8;
        else if (type->ts & T_FUNC)
            error(ERROR_ILLFUNC);
        else if (type->ts & T_VOID)
            error(ERROR_ILLVOID);
        else if (type->ts & T_TAG) {
            if (type->tag->ss & S_DEFINED)
                size *= type->tag->i;
            else
                error(ERROR_INCOMPLT);
        } else if (type->ts & T_ARRAY) {
            if (type->nr_elements == 0)
                error(ERROR_INCOMPLT);
            else
                size *= type->nr_elements;
        } 

        if (size > MAX_SIZE) error(ERROR_TYPESIZE);
        if (type->ts & T_PTR) break;
        type = type->next;
    }

    return size;
}

int
align_of(struct type * type)
{
    int align = 1;

    while (type) {
        if (type->ts & T_ARRAY)
            /* */ ;
        else if (type->ts & T_IS_BYTE) 
            align = 1;
        else if (type->ts & T_IS_WORD) 
            align = 2;
        else if (type->ts & T_IS_DWORD) 
            align = 4;
        else if (type->ts & T_IS_QWORD)
            align = 8;
        else if (type->ts & T_FUNC)
            error(ERROR_ILLFUNC);
        else if (type->ts & T_VOID)
            error(ERROR_ILLVOID);
        else if (type->ts & T_TAG) {
            if (type->tag->ss & S_DEFINED)
                align = type->tag->align;
            else
                error(ERROR_INCOMPLT);
        } 

        if (type->ts & T_PTR) break;
        type = type->next;
    } 

    return align;
}

/* perform some preliminary checks on a type:
     1. functions can't return arrays or functions,
     2. array elements can't be functions or void,
     3. only the first index of an array can be unbounded. */

void
validate_type(struct type * type)
{
    while (type) {
        if (type->ts & T_ARRAY) {
            if (type->next->ts & T_ARRAY) {
                if (type->next->nr_elements == 0)
                    error(ERROR_ILLARRAY);
            } else if (type->next->ts & T_FUNC) {
                error(ERROR_ILLFUNC);
            } else if (type->next->ts & T_VOID) {
                error(ERROR_ILLVOID);
            }
        } else if (type->ts & T_FUNC) {
            if (type->next->ts & (T_ARRAY | T_FUNC))
                error(ERROR_RETURN);
        }
        type = type->next;
    }
}

/* adjust and validate the type of a formal argument.

   mode is ARGUMENT_TYPE_OLD or ARGUMENT_TYPE_NEW, indicating
   an old-style function definition argument, or a new-style
   prototype or definition argument. 

     1. arrays become pointers, 
     2. functions become pointers to function,
     3. floats become doubles (ARGUMENT_TYPE_OLD only) */

struct type *
argument_type(struct type * type, int mode)
{
    if (type->ts & T_ARRAY) {
        type->ts &= ~T_ARRAY;
        type->ts |= T_PTR;
    }

    if (type->ts & T_FUNC) type = splice_types(new_type(T_PTR), type);

    if (mode == ARGUMENT_TYPE_OLD) {
        if (type->ts & T_FLOAT) {
            type->ts &= ~T_FLOAT;
            type->ts |= T_DOUBLE;
        }
    }

    return type;
}

#ifndef NDEBUG

static struct
{
    int ts;
    char *text;
} ts[] = {
    { T_CONST, "const " },
    { T_VOLATILE, "volatile " },
    { T_VOID, "void" },
    { T_CHAR, "char" },
    { T_UCHAR, "unsigned char" },
    { T_SCHAR, "signed char" },
    { T_SHORT, "short" },
    { T_USHORT, "unsigned short" },
    { T_INT, "int" },
    { T_UINT, "unsigned int" },
    { T_LONG, "long" },
    { T_ULONG, "unsigned long" },
    { T_FLOAT, "float" },
    { T_DOUBLE, "double" },
    { T_LDOUBLE, "long double" },
    { T_PTR, "pointer to " },
    { T_FUNC, "function " },
    { T_ARRAY, "array[" }
};

#define NR_TFLAGS (sizeof(ts)/sizeof(*ts))

void
debug_type(struct type * type)
{
    struct symbol * symbol;
    int             i;

    while (type) {
        for (i = 0; i < NR_TFLAGS; i++)
            if (ts[i].ts & type->ts)
                fprintf(stderr, "%s", ts[i].text);

        if (type->ts & T_ARRAY) {
            if (type->nr_elements) fprintf(stderr, "%d", type->nr_elements);
            fprintf(stderr,"] of ");
        }

        if (type->ts & T_FUNC) {
            fputc('(', stderr);

            if (type->proto) {
                if (type->proto->ps & P_STALE) fprintf(stderr, "*STALE* ");
                if (symbol = type->proto->args) {
                    i = 0;

                    while (symbol) {
                        if (i) fprintf(stderr, ", ");
                        debug_type(symbol->type);
                        symbol = symbol->list;
                        ++i;
                    }
                } else fprintf(stderr, "void");

                if (type->proto->ps & P_VARIADIC) 
                    fprintf(stderr, ", ...");
            } 

            fputc(')', stderr);
            fprintf(stderr, " returning ");
        }

        if (type->ts & T_TAG) {
            if (type->tag->ss & S_STRUCT) fprintf(stderr, "struct");
            if (type->tag->ss & S_UNION) fprintf(stderr, "union");
            if (type->tag->id)
                fprintf(stderr, " '%s'", type->tag->id->data);
            else
                fprintf(stderr, " @ %p", type->tag);
        }

        if (type->ts & T_FIELD) 
            fprintf(stderr, "[FIELD size %d shift %d] ", T_GET_SIZE(type->ts), T_GET_SHIFT(type->ts));

        type = type->next;
    }
}

#endif

