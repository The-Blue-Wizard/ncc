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

/* a token is comprised of a class and possibly some additional data
   in 'u' (see KK_* comments for details). */

struct token
{
    int kk;    

    union
    {
        long            i;
        double          f;     
        struct string * text;
    } u;
};

/* token classes are for the most part arbitrary values, but 
   some bits in the values have special meanings to simplify
   the parser.

   kk[7:0] = unique to the token
   kk[19:8] = binary precedence level (KK_PREC_*)
   kk[31:20] = type specifier (KK_TS_*) 

   the "pseudo tokens" are used internally by the lexer to track
   file position for error reporting (including # line directives).
   they are never seen by the parser. */

#define KK_PREC(x)              (0x00000100 << (x))
#define KK_TS(x)                (0x00100000 << (x))

#define KK_PREC_MASK            0x000FFF00
#define KK_TS_MASK              0xFFF00000

#define KK_PREC_MUL             0
#define KK_PREC_ADD             1
#define KK_PREC_SHIFT           2
#define KK_PREC_REL             3
#define KK_PREC_EQ              4
#define KK_PREC_AND             5
#define KK_PREC_XOR             6
#define KK_PREC_OR              7
#define KK_PREC_LAND            8
#define KK_PREC_LOR             9 

#define KK_TS_VOID              KK_TS(0)
#define KK_TS_CHAR              KK_TS(1)
#define KK_TS_SHORT             KK_TS(2)
#define KK_TS_INT               KK_TS(3)
#define KK_TS_LONG              KK_TS(4)
#define KK_TS_SIGNED            KK_TS(5)
#define KK_TS_UNSIGNED          KK_TS(6)
#define KK_TS_FLOAT             KK_TS(7)
#define KK_TS_DOUBLE            KK_TS(8)

#define KK_NONE                 0

#define KK_IDENT                1       /* identifier: u.text */
#define KK_STRLIT               2       /* string literal: u.text */
#define KK_FCON                 3       /* float constant: u.f */
#define KK_DCON                 4       /* double constant: u.f */
#define KK_LDCON                5       /* long double constant: u.f */
#define KK_ICON                 6       /* int constant: u.i */
#define KK_UCON                 7       /* unsigned constant: u.i */
#define KK_LCON                 8       /* long constant: u.i */
#define KK_ULCON                9       /* unsigned long constant: u.i */

#define KK_NL                   10      /* pseudo tokens */
#define KK_HASH                 11

#define KK_LPAREN   /* ( */     20      /* operators/punctuators */
#define KK_RPAREN   /* ) */     21     
#define KK_LBRACK   /* [ */     22      
#define KK_RBRACK   /* ] */     23      
#define KK_LBRACE   /* { */     24     
#define KK_RBRACE   /* } */     25      
#define KK_DOT      /* . */     26     
#define KK_XOR      /* ^ */     (27 | KK_PREC(KK_PREC_XOR))
#define KK_COMMA    /* , */     28  
#define KK_COLON    /* : */     29 
#define KK_SEMI     /* ; */     30      
#define KK_QUEST    /* ? */     31      
#define KK_TILDE    /* ~ */     32      
#define KK_ARROW    /* -> */    33   
#define KK_INC      /* ++ */    34      
#define KK_DEC      /* -- */    35     
#define KK_BANG     /* ! */     36     
#define KK_DIV      /* / */     (37 | KK_PREC(KK_PREC_MUL))
#define KK_STAR     /* * */     (38 | KK_PREC(KK_PREC_MUL))
#define KK_PLUS     /* + */     (39 | KK_PREC(KK_PREC_ADD))
#define KK_MINUS    /* - */     (40 | KK_PREC(KK_PREC_ADD))
#define KK_GT       /* > */     (41 | KK_PREC(KK_PREC_REL))
#define KK_SHR      /* >> */    (42 | KK_PREC(KK_PREC_SHIFT))
#define KK_GTEQ     /* >= */    (43 | KK_PREC(KK_PREC_REL))
#define KK_SHREQ    /* >>= */   44   
#define KK_LT       /* < */     (45 | KK_PREC(KK_PREC_REL))
#define KK_SHL      /* << */    (46 | KK_PREC(KK_PREC_SHIFT))
#define KK_LTEQ     /* <= */    (47 | KK_PREC(KK_PREC_REL))
#define KK_SHLEQ    /* <<= */   48 
#define KK_AND      /* & */     (49 | KK_PREC(KK_PREC_AND))
#define KK_ANDAND   /* && */    (50 | KK_PREC(KK_PREC_LAND))
#define KK_ANDEQ    /* &= */    51  
#define KK_BAR      /* | */     (52 | KK_PREC(KK_PREC_OR))
#define KK_BARBAR   /* || */    (53 | KK_PREC(KK_PREC_LOR)) 
#define KK_BAREQ    /* |= */    54
#define KK_MINUSEQ  /* -= */    55
#define KK_PLUSEQ   /* += */    56
#define KK_STAREQ   /* *= */    57
#define KK_DIVEQ    /* /= */    58
#define KK_EQEQ     /* == */    (59 | KK_PREC(KK_PREC_EQ))
#define KK_BANGEQ   /* != */    (60 | KK_PREC(KK_PREC_EQ))
#define KK_MOD      /* % */     (61 | KK_PREC(KK_PREC_MUL))
#define KK_MODEQ    /* %= */    62
#define KK_XOREQ    /* ^= */    63    
#define KK_EQ       /* = */     64  
#define KK_ELLIP    /* ... */   65

#define KK_AUTO                 70          /* keywords */
#define KK_BREAK                71      
#define KK_CASE                 72
#define KK_CHAR                 (73 | KK_TS_CHAR)
#define KK_CONST                74
#define KK_CONTINUE             75
#define KK_DEFAULT              76
#define KK_DO                   77
#define KK_DOUBLE               (78 | KK_TS_DOUBLE)
#define KK_ELSE                 79
#define KK_ENUM                 80
#define KK_EXTERN               81
#define KK_FLOAT                (82 | KK_TS_FLOAT)
#define KK_FOR                  83
#define KK_GOTO                 84
#define KK_IF                   85
#define KK_INT                  (86 | KK_TS_INT)
#define KK_LONG                 (87 | KK_TS_LONG)
#define KK_REGISTER             88
#define KK_RETURN               89
#define KK_SHORT                (90 | KK_TS_SHORT)
#define KK_SIGNED               (91 | KK_TS_SIGNED)
#define KK_SIZEOF               92
#define KK_STATIC               93
#define KK_STRUCT               94
#define KK_SWITCH               95
#define KK_TYPEDEF              96
#define KK_UNION                97
#define KK_UNSIGNED             (98 | KK_TS_UNSIGNED)
#define KK_VOID                 (99 | KK_TS_VOID)
#define KK_VOLATILE             100
#define KK_WHILE                101

