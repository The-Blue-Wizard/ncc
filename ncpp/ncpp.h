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

extern FILE * output_file;

/* struct vstring represents a variable-length string */

struct vstring
{
    char * data;
    int    length;
    int    capacity;
};

struct macro
{
    struct vstring * name;
    struct list *    arguments;
    struct list *    replacement;
    struct macro *   link;
    int              predefined;
};

#define MACRO_LOOKUP_NORMAL 0
#define MACRO_LOOKUP_CREATE 1

#define MACRO_REPLACE_ONCE   0
#define MACRO_REPLACE_REPEAT 1

struct input
{
    struct vstring * path;
    FILE *           file;
    int              line_number;
    struct input *   stack_link;
};

extern struct input * input_stack;

#define INPUT_LINE_NORMAL  0
#define INPUT_LINE_LIMITED 1

#define INPUT_INCLUDE_LOCAL  0   
#define INPUT_INCLUDE_SYSTEM 1

struct token
{
    int            class;

    struct token * previous;
    struct token * next;

    union
    {
        struct vstring * text;
        int              argument_no;
        int              ascii;
        long             int_value;
        unsigned long    unsigned_value;
    } u;
};

struct list
{
    int             count;
    struct token *  first;
    struct token *  last;
};

/* token classes. be careful when changing this list- the values 
   must match the indices into the token_text[] array in token.c */

#define TOKEN_SPACE         0       /* u.ascii: whitespace (except newline) */
#define TOKEN_INT           1       /* u.int_value: integer (in expression) */
#define TOKEN_UNSIGNED      2       /* u.unsigned_value: unsigned (in expression) */ 
#define TOKEN_ARG           3       /* u.argument_no: placeholder for function-like macro argument */
#define TOKEN_UNKNOWN       4       /* u.ascii: any char in input not otherwise accounted for */
#define TOKEN_STRING        5       /* u.text: string literal */
#define TOKEN_CHAR          6       /* u.text: char constant */
#define TOKEN_NUMBER        7       /* u.text: preprocessing number */
#define TOKEN_NAME          8       /* u.text: an identifier subject to macro replacement */
#define TOKEN_EXEMPT_NAME   9       /* u.text: an identifier NOT subject to macro replacement */

#define TOKEN_GT            10      /* > */        
#define TOKEN_LT            11      /* < */         
#define TOKEN_GTEQ          12      /* >= */
#define TOKEN_LTEQ          13      /* <= */
#define TOKEN_SHL           14      /* << */
#define TOKEN_SHLEQ         15      /* <<= */
#define TOKEN_SHR           16      /* >> */
#define TOKEN_SHREQ         17      /* >>= */
#define TOKEN_EQ            18      /* = */
#define TOKEN_EQEQ          19      /* == */

#define TOKEN_NOTEQ         20      /* != */    
#define TOKEN_PLUS          21      /* + */         
#define TOKEN_PLUSEQ        22      /* += */
#define TOKEN_INC           23      /* ++ */
#define TOKEN_MINUS         24      /* - */
#define TOKEN_MINUSEQ       25      /* -= */
#define TOKEN_DEC           26      /* -- */
#define TOKEN_ARROW         27      /* -> */
#define TOKEN_LPAREN        28      /* ( */
#define TOKEN_RPAREN        29      /* ) */

#define TOKEN_LBRACK        30      /* [ */       
#define TOKEN_RBRACK        31      /* ] */         
#define TOKEN_LBRACE        32      /* { */
#define TOKEN_RBRACE        33      /* } */
#define TOKEN_COMMA         34      /* , */
#define TOKEN_DOT           35      /* . */
#define TOKEN_QUEST         36      /* ? */
#define TOKEN_COLON         37      /* : */
#define TOKEN_SEMI          38      /* ; */
#define TOKEN_OR            39      /* | */

#define TOKEN_OROR          40      /* || */      
#define TOKEN_OREQ          41      /* |= */      
#define TOKEN_AND           42      /* & */
#define TOKEN_ANDAND        43      /* && */
#define TOKEN_ANDEQ         44      /* &= */
#define TOKEN_MUL           45      /* * */
#define TOKEN_MULEQ         46      /* *= */
#define TOKEN_MOD           47      /* % */
#define TOKEN_MODEQ         48      /* %= */
#define TOKEN_XOR           49      /* ^ */

#define TOKEN_XOREQ         50      /* ^= */   
#define TOKEN_NOT           51      /* ! */         
#define TOKEN_HASH          52      /* # */
#define TOKEN_HASHHASH      53      /* ## (impotent) */
#define TOKEN_DIV           54      /* / */
#define TOKEN_DIVEQ         55      /* /= */
#define TOKEN_TILDE         56      /* ~ */
#define TOKEN_ELLIPSIS      57      /* ... */
#define TOKEN_PASTE         58      /* ## (in macro replacement list) */

#define LIST_TRIM_LEADING       0x00000001
#define LIST_TRIM_TRAILING      0x00000002
#define LIST_TRIM_EDGES         (LIST_TRIM_LEADING | LIST_TRIM_TRAILING)
#define LIST_TRIM_FOLD          0x00000004
#define LIST_TRIM_STRIP         0x00000008

#define LIST_GLUE_RAW       0
#define LIST_GLUE_STRINGIZE 1

#define SKIP_SPACES(t) while ((t) && ((t)->class == TOKEN_SPACE)) ((t) = (t)->next)

extern void             input_open(struct vstring *);
extern void             input_include_directory(char *);
extern void             input_include(struct vstring *, int);
extern struct token   * token_new(int);
extern void             token_free(struct token *);
extern struct token   * token_copy(struct token *);
extern void             token_print(struct token *, FILE *);
extern struct token   * token_paste(struct token *, struct token *);
extern int              token_equal(struct token *, struct token *);
extern void             token_convert_number(struct token *);
extern struct list    * list_new(void);
extern void             list_free(struct list *);
extern void             list_clear(struct list *);
extern struct token   * list_unlink(struct list *, struct token *);
extern struct token   * list_delete(struct list *, struct token *);
extern struct list    * list_copy(struct list *);
extern int              list_equal(struct list *, struct list *);
extern void             list_insert(struct list *, struct token *, struct token *);
extern struct vstring * list_glue(struct list *, int);
extern void             list_move(struct list *, struct list *, int, struct token *);
extern void             list_trim(struct list *, int);
extern void             list_cut(struct list *, struct token *);
extern struct vstring * input_line(int);
extern struct macro   * macro_lookup(struct vstring *, int);
extern void             macro_define(struct vstring *, struct list *, struct list *);
extern void             macro_undef(struct vstring *);
extern void             macro_option(char *);
extern void             macro_predefine(void);
extern void             macro_replace(struct list *, int);
extern struct vstring * vstring_new(char *);
extern void             vstring_free(struct vstring *);
extern struct vstring * vstring_copy(struct vstring *);
extern struct vstring * vstring_from_literal(struct vstring *);
extern int              vstring_equal(struct vstring *, struct vstring *);
extern int              vstring_equal_s(struct vstring *, char *);
extern void             vstring_puts(struct vstring *, char *);
extern void             vstring_putc(struct vstring *, int);
extern unsigned         vstring_hash(struct vstring *);
extern void             vstring_rubout(struct vstring *);
extern void             vstring_concat(struct vstring *, struct vstring *);
extern void             directive(struct list *);
extern void           * safe_malloc(int);
extern void             fail(char *, ...);
extern void             out(char *, ...);
extern void             tokenize(struct vstring *, struct list *);

