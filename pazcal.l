%{
#include <stdio.h>
#include <string.h>

#include "error.h"
#include "general.h"
#include "symbol.h"
#include "pazcal.tab.h"

#define T_eof          0

char* removeEscaped(char*);
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

{L}({ALPH}|_)*  { yylval.str = (char *) new( sizeof(char)  * (strlen(yytext) + 1) );
                  strcpy(yylval.str, yytext);
                  return T_id; }
(0|[1-9]{D}*)   { yylval.integer = atoi(yytext);
                  return T_int_const; }
{D}+\.{D}+((e|E)(\+|\-)?{D}+)?   { yylval.real = atof(yytext);
                                   return T_float_const; }

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

\'({PRINTABLE}|{ESCAPESEQ})\'    { yylval.chr = * (char *) (removeEscaped(yytext) + 1);
                                   return T_char_const; }
\"([^\\\"\n]|{ESCAPESEQ})*\"     { char* s = removeEscaped(yytext);
                                   int len = strlen(s);
                                   s[len-1] = '\0';
                                   yylval.str = s + 1;
                                   return T_string_literal; }

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

char* removeEscaped(char* s) {
   int i, j;
   char *p;

   p = new( sizeof(char) * (strlen(s) + 1) );
   for (i = j = 0; s[i] != '\0'; i++, j++) {
      if (s[i] == '\\') {
         switch (s[i+1]) {
            case 'n':
               p[j] = '\n';
               break;
            case 't':
               p[j] = '\t';
               break;
            case 'r':
               p[j] = '\r';
               break;
            case '0':
               p[j] = '\0';
               break;
            case '\\':
               p[j] = '\\';
               break;
            case '\'':
               p[j] = '\'';
               break;
            case '\"':
               p[j] = '\"';
               break;
            case '\0':
               i--;
               break;
            default:
               p[j] = s[i+1];
               break;
         }
         i++;
      } else p[j] = s[i];
   }

   p[j] = '\0';
   return p;
}
