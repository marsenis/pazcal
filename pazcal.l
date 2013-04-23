%{
#include <stdio.h>

#include "error.h"
#include "general.h"
#include "pazcal.tab.h"

#define T_eof          0

/*
int linecount = 1;
char filename[] = "stdin";
*/
%}

D         [0-9]
L         [a-zA-Z]
ALPH      [0-9a-zA-Z]
W         [ \t\n\r]
PRINTABLE [ -~]
ESCAPESEQ (\\n|\\t|\\r|\\0|\\\\|\\\'|\\\")

%x COMMENT

%%

"and"           { return T_and      ;}
"bool"          { return T_bool     ;}
"break"         { return T_break    ;}
"case"          { return T_case     ;}
"char"          { return T_char     ;}
"const"         { return T_const    ;}
"continue"      { return T_continue ;}
"default"       { return T_default  ;}
"do"            { return T_do       ;}
"DOWNTO"        { return T_DOWNTO   ;}
"else"          { return T_else     ;}
"false"         { return T_false    ;}
"FOR"           { return T_FOR      ;}
"FORM"          { return T_FORM     ;}
"FUNC"          { return T_FUNC     ;}
"if"            { return T_if       ;}
"int"           { return T_int      ;}
"MOD"           { return '%'        ;}
"NEXT"          { return T_NEXT     ;}
"not"           { return '!'        ;}
"or"            { return T_or       ;}
"PROC"          { return T_PROC     ;}
"PROGRAM"       { return T_PROGRAM  ;}
"REAL"          { return T_REAL     ;}
"return"        { return T_return   ;}
"STEP"          { return T_STEP     ;}
"switch"        { return T_switch   ;}
"TO"            { return T_TO       ;}
"true"          { return T_true     ;}
"while"         { return T_while    ;}
"WRITE"         { return T_WRITE    ;}
"WRITELN"       { return T_WRITELN  ;}
"WRITESP"       { return T_WRITESP  ;}
"WRITESPLN"     { return T_WRITESPLN;}

{L}({ALPH}|_)*  { return T_id; }
(0|[1-9]{D}*)   { return T_int_const; }
{D}+\.{D}+((e|E)(\+|\-)?{D}+)?   { return T_float_const; }

[\>\<\+\-\*\/\%\!\=\&\;\.\(\)\:\,\[\]\{\}]   { return yytext[0]; }
"==" { return T_eq; }
"!=" { return T_neq; }
">=" { return T_geq; }
"<=" { return T_leq; }
"&&" { return T_and; }
"||" { return T_or; }
"++" { return T_plusplus; }
"--" { return T_minusminus; }
"+=" { return T_pluseq; }
"-=" { return T_minuseq; }
"*=" { return T_multeq; }
"/=" { return T_diveq; }
"%=" { return T_modeq; }

[ \t]    { /* empty */ }
\/\/.*   { /* empty */ } /* one line comments */

\'({PRINTABLE}|{ESCAPESEQ})\'    { return T_char_const; }
\"([^\\\"\n]|{ESCAPESEQ})*\"     { return T_string_literal; }

"\/*"            { BEGIN(COMMENT); }
<COMMENT>"*\/"   { BEGIN(INITIAL); }
<COMMENT>\n      { linecount++;    }
<COMMENT>\*      { /* empty */     }
<COMMENT>[^*\n]+ { /* empty */     }

\r   { /* empty */ }
\n   { linecount++; }

.        { warning("illegal character '%c'. Ignoring it...", yytext[0]); }
<<EOF>>  { return T_eof; }

%%
