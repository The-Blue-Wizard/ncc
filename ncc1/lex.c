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

#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#include <ctype.h>
#include <float.h>
#include "ncc1.h"

static int    yych;         /* current input character */

#define yynext() (yych = getc(yyin))

/* we maintain a dynamically-growing token buffer for
   those token classes with interesting text. */

#define YY_INCREMENT 128    /* grow in these increments */

static int    yycap;        /* capacity of token buffer */
static int    yylen;        /* length of current token */
static char * yybuf;        /* token buffer */
static char * yytext;       /* pointer into yytext */

/* normally, adjacent string literals are concatenated. setting
   this flag prevents that when processing # line directives. */

static int    directive;

/* a pushback buffer for peek(). priming this with KK_NL is a trick
   that makes the logic in lex() work properly on the first call. */

static struct token next = { KK_NL };  

/* process a floating-point constant */

static int
fcon(void)
{
    char * endptr;
    int    kk;

    errno = 0;

    if (toupper(yych) == 'F') {
        kk = KK_FCON;
        yynext();
    } else {
        if (toupper(yych) == 'L') {
            yynext();
            kk = KK_LDCON;
        } else
            kk = KK_DCON;
    }

    token.u.f = strtod(yytext, &endptr);
    if (errno == ERANGE) error(ERROR_FRANGE);
    if (errno || *endptr) error(ERROR_BADFCON);

    if ((kk == KK_FCON) && ((token.u.f < FLT_MIN) || (token.u.f > FLT_MAX)))
        error(ERROR_FRANGE);

    return kk;
}

/* returns the next character of a char constant or string literal,
   interpreting escape codes. */

#define ISODIGIT(c)     (isdigit(c) && ((c) < '8'))
#define DIGIT_VALUE(c)  ((c) - '0')

static int
escape(void)
{
    int c;

    if (*yytext == '\\') {
        yytext++;
        switch (*yytext) {
        case 'b': c = '\b'; yytext++; break;
        case 'f': c = '\f'; yytext++; break;
        case 'n': c = '\n'; yytext++; break;
        case 'r': c = '\r'; yytext++; break;
        case 't': c = '\t'; yytext++; break;

        case '0': case '1': case '2': case '3':
        case '4': case '5': case '6': case '7':
            c = DIGIT_VALUE(*yytext);
            yytext++;
            if (ISODIGIT(*yytext)) {
                c <<= 3;
                c += DIGIT_VALUE(*yytext);
                yytext++;
                if (ISODIGIT(*yytext)) {
                    c <<= 3;
                    c += DIGIT_VALUE(*yytext);
                    yytext++;
                }
            }
            if (c > UCHAR_MAX) error(ERROR_ESCAPE);
            break;

        default:
            c = *yytext++;
        }
    } else 
        c = *yytext++;

    return c;
}

static int
ccon(void)
{
    int value = 0;

    yytext++;
    while (*yytext != '\'') {
        if (value & 0xFF000000) error(ERROR_CRANGE);
        value <<= 8;
        value += escape();
    }

    token.u.i = value;
    return KK_ICON;
}

/* interpret/convert a string literal */

static int
strlit(void)
{
    char *  data = yytext;
    char *  cp = yytext;
    int     length = 0;

    do {
        ++yytext;   /* opening quote */

        while (*yytext != '\"') {
            *cp++ = escape();
            length++;
        }

        ++yytext;   /* closing quote */
    } while (*yytext);

    token.u.text = stringize(data, length);
    return KK_STRLIT;
}

/* stash character 'c' in the token buffer, growing
   the buffer if necessary. */

static void
yystash(int c)
{
    char * new_yybuf;

    if (yylen == yycap) {
        new_yybuf = allocate(yycap + YY_INCREMENT + 1);
        memcpy(new_yybuf, yybuf, yycap);
        free(yybuf);
        yybuf = new_yybuf;
        yytext = yybuf;
    }

    yybuf[yylen++] = c;
    yybuf[yylen] = 0;
}

/* deal with "delimited" tokens, i.e., character constants
   and string literals; delimiter is ' or ", respectively. */

static void
delimited(int delimiter)
{
    int backslash = 1; /* fake out first loop */

    while ((yych >= 0) && (yych != '\n')) {
        yystash(yych);
        if ((yych == delimiter) && !backslash) break;
        backslash = (yych == '\\') && !backslash;
        yynext();
    }

    if ((yych < 0) || (yych == '\n')) 
        error((delimiter == '"') ? ERROR_UNTERM : ERROR_BADCCON);

    yynext();   /* eat closing delimiter */
}

/* called by main() after setting 'yyin' but before the first
   call to lex() to initialize the scanner. */

static struct
{
    char * yytext;
    int    kk;
} keyword[] =
{
    { "auto", KK_AUTO }, { "break", KK_BREAK }, { "case", KK_CASE },
    { "char", KK_CHAR }, { "const", KK_CONST }, { "continue", KK_CONTINUE }, 
    { "default", KK_DEFAULT }, { "do", KK_DO }, { "double", KK_DOUBLE }, 
    { "else", KK_ELSE }, { "enum", KK_ENUM }, { "extern", KK_EXTERN }, 
    { "float", KK_FLOAT }, { "for", KK_FOR }, { "goto", KK_GOTO }, { "if", KK_IF }, 
    { "int", KK_INT }, { "long", KK_LONG }, { "register", KK_REGISTER }, 
    { "return", KK_RETURN }, { "short", KK_SHORT }, { "signed", KK_SIGNED },
    { "sizeof", KK_SIZEOF }, { "static", KK_STATIC }, { "struct", KK_STRUCT }, 
    { "switch", KK_SWITCH }, { "typedef", KK_TYPEDEF }, { "union", KK_UNION }, 
    { "unsigned", KK_UNSIGNED }, { "void", KK_VOID }, { "volatile", KK_VOLATILE },
    { "while", KK_WHILE }
};

#define NR_KEYWORDS (sizeof(keyword)/sizeof(*keyword))

void
yyinit(void)
{
    struct string * k;
    int             i;

    for (i = 0; i < NR_KEYWORDS; ++i) {
        k = stringize(keyword[i].yytext, strlen(keyword[i].yytext));
        k->token = keyword[i].kk;
    }

    yynext();
}

/* determine the type and value of integral constants. */

static int
icon(void)
{
    unsigned long value = 0;
    char *        endptr;
    int           kk;

    errno = 0;
    kk = KK_ICON;
    value = strtoul(yytext, &endptr, 0);
    if (errno == ERANGE) error(ERROR_IRANGE);

    if (toupper(yych) == 'L') {
        kk = KK_LCON;
        yynext();
        if (toupper(yych) == 'U') {
            yynext();
            kk = KK_ULCON;
        } else {
            if (value > LONG_MAX) 
                kk = KK_ULCON;
        }
    } else if (toupper(yych) == 'U') {
        kk = KK_UCON;
        yynext();
        if (toupper(yych) == 'L') {
            yynext();
            kk = KK_ULCON;
        } else {
            if (value > UINT_MAX)
                kk = KK_ULCON;
        }
    } else { /* unsuffixed */
        if (value > INT_MAX) {
            if (*yytext == '0') 
                kk = KK_UCON;
            else
                kk = KK_LCON;
        }

        if (value > UINT_MAX) kk = KK_LCON;
        if (value > LONG_MAX) kk = KK_ULCON;
    }

    if (errno || *endptr) error(ERROR_BADICON);
    token.u.i = value;
    return kk;
}

/* just like a flex auto-generated yylex, except that we don't
   bother stashing tokens in 'yytext' if the text is irrelevant. */

static int
yylex(void)
{
    int newlines;   /* for string literals */

    while (isspace(yych) && (yych != '\n'))
        yynext();

    yylen = 0;
    yytext = yybuf;

    switch (yych)
    {
    case -1:    return KK_NONE;
    case '\n':  yynext(); return KK_NL;
    case '#':   yynext(); return KK_HASH;
    case '(':   yynext(); return KK_LPAREN;
    case ')':   yynext(); return KK_RPAREN;
    case '[':   yynext(); return KK_LBRACK;
    case ']':   yynext(); return KK_RBRACK;
    case '{':   yynext(); return KK_LBRACE;
    case '}':   yynext(); return KK_RBRACE;
    case ',':   yynext(); return KK_COMMA;
    case ':':   yynext(); return KK_COLON;
    case ';':   yynext(); return KK_SEMI;
    case '?':   yynext(); return KK_QUEST;
    case '~':   yynext(); return KK_TILDE;

    case '!':
        yynext();

        if (yych == '=') {
            yynext();
            return KK_BANGEQ;
        }

        return KK_BANG;

    case '=':
        yynext();

        if (yych == '=') {
            yynext();
            return KK_EQEQ;
        }

        return KK_EQ;

    case '^':
        yynext();

        if (yych == '=') {
            yynext();
            return KK_XOREQ;
        }

        return KK_XOR;

    case '*':
        yynext();

        if (yych == '=') {
            yynext();
            return KK_STAREQ;
        }

        return KK_STAR;

    case '%':
        yynext();

        if (yych == '=') {
            yynext();
            return KK_MODEQ;
        }

        return KK_MOD;

    case '/':
        yynext();

        if (yych == '=') {
            yynext();
            return KK_DIVEQ;
        }

        return KK_DIV;

    case '+':
        yynext();

        if (yych == '+') {
            yynext();
            return KK_INC;
        } else if (yych == '=') {
            yynext();
            return KK_PLUSEQ;
        }

        return KK_PLUS;

    case '-':
        yynext();

        if (yych == '-') {
            yynext();
            return KK_DEC;
        } else if (yych == '=') {
            yynext();
            return KK_MINUSEQ;
        } else if (yych == '>') {
            yynext();
            return KK_ARROW;
        }

        return KK_MINUS;

    case '&':
        yynext();

        if (yych == '&') {
            yynext();
            return KK_ANDAND;
        } else if (yych == '=') {
            yynext();
            return KK_ANDEQ;
        }

        return KK_AND;

    case '|':
        yynext();

        if (yych == '|') {
            yynext();
            return KK_BARBAR;
        } else if (yych == '=') {
            yynext();
            return KK_BAREQ;
        }

        return KK_BAR;

    case '.':
        yynext();

        if (isdigit(yych)) {
            /* whoops, it's a float */
            ungetc(yych, yyin);
            yych = '.';
            break;
        } 

        if (yych == '.') {
            yynext();

            if (yych == '.') {
                yynext();
                return KK_ELLIP;
            }

            error(ERROR_LEXICAL);  
        }

        return KK_DOT;

    case '>':
        yynext();

        if (yych == '>') {
            yynext();
            if (yych == '=') {
                yynext();
                return KK_SHREQ;
            }
            return KK_SHR;
        } else if (yych == '=') {
            yynext();
            return KK_GTEQ;
        }

        return KK_GT;

    case '<':
        yynext();

        if (yych == '<') {
            yynext();
            if (yych == '=') {
                yynext();
                return KK_SHLEQ;
            }
            return KK_SHL;
        } else if (yych == '=') {
            yynext();
            return KK_LTEQ;
        }

        return KK_LT;

    case '\'':
        delimited('\'');
        return ccon();

    case '\"':
        /* adjacent string literals are treated as one string literal.
           these tokens can span lines, so we need special handling */

        do {
            delimited('\"');
            newlines = 0;

            while (isspace(yych) && !directive) {
                if (yych == '\n') {
                    ++newlines;
                    ++line_number;
                }
                yynext();
            }
        } while ((yych == '\"') && !directive);

        if (newlines) {
            --line_number;
            ungetc(yych, yyin);
            yych = '\n';
        }

        return strlit();

    default: /* fall through */ ;
    }

    /* identifiers/keywords */

    if (isalpha(yych) || (yych == '_')) {
        while (isalnum(yych) || (yych == '_')) {
            yystash(yych);
            yynext();
        }

        token.u.text = stringize(yytext, yylen);
        return token.u.text->token;
    }

    /* numbers */

    if (isdigit(yych) || (yych == '.')) {
        if (yych == '0') {
            yystash(yych);
            yynext();

            if (toupper(yych) == 'X') {
                yystash(yych);
                yynext();

                while (isxdigit(yych)) {
                    yystash(yych);
                    yynext();
                }

                return icon();
            }
        }

        while (isdigit(yych)) {
            yystash(yych);
            yynext();
        }

        if ((yych == '.') || (toupper(yych) == 'E')) {
            if (yych == '.') {
                yystash(yych);
                yynext();

                while (isdigit(yych)) {
                    yystash(yych);
                    yynext();
                }
            }


            if (toupper(yych) == 'E') {
                yystash(yych);
                yynext();

                if (yych == '-') {
                    yystash(yych);
                    yynext();
                } else if (yych == '+') {
                    yystash(yych);
                    yynext();
                }

                while (isdigit(yych)) {
                    yystash(yych);
                    yynext();
                }
            }

            return fcon();
        } else 
            return icon();
    }

    error(ERROR_LEXICAL);
}

/* peek returns some information about the next token in the 
   input. the parser needs this in a few random circumstances 
   where the grammar is slightly irregular. */

int
peek(struct string ** id)
{
    struct token saved;
    int          kk;

    memcpy(&saved, &token, sizeof(struct token));
    lex();
    kk = token.kk;
    if ((kk == KK_IDENT) && (id)) *id = token.u.text;
    memcpy(&next, &token, sizeof(struct token));
    memcpy(&token, &saved, sizeof(struct token));
    return kk;
}

/* the lexical analyzer functions as a three-stage pipeline:

    lex() 
        called by the parser to retrieve the next token.
        this filters the output of ylex(), stripping pseudo 
        tokens, after using them to track the input file
        location by counting newlines and processing directives.

    ylex()
        sits between lex() and yylex(), reinjecting
        tokens buffered by peek()

    yylex()
        the scanner proper, divides the input into tokens */

static void
ylex(void)
{
    if (next.kk == KK_NONE) {
        token.kk = yylex();
    } else {
        memcpy(&token, &next, sizeof(struct token));
        next.kk = KK_NONE;
    }
}

void
lex(void)
{
    ylex();
    while (token.kk == KK_NL) {
        line_number++;
        ylex();
        while (token.kk == KK_HASH) {
            directive = 1;
            ylex();

            if (token.kk == KK_ICON) {
                line_number = token.u.i;
                ylex();
                if (token.kk == KK_STRLIT) {
                    input_name = token.u.text;
                    ylex();
                }
            }

            /* there's a good chance the error will be 
               reported in the wrong location, seeing as
               the error is in grokking the location. */

            if (token.kk != KK_NL) error(ERROR_DIRECTIVE);
            directive = 0;
            ylex();
        }
    }
}

/* simple parsing helper functions.
   these should be self-explanatory. */

void
expect(int kk)
{
    if (token.kk != kk) error(ERROR_SYNTAX);
}

void
prohibit(int kk)
{
    if (token.kk == kk) error(ERROR_SYNTAX);
}

void
match(int kk)
{
    expect(kk);
    lex();
}

