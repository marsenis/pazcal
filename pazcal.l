/* TODO: bison should take care of token ids */
%{
#define T_eof          0
#define T_and          257
#define T_bool         258
#define T_break        259
#define T_case         260
#define T_char         261
#define T_const        262
#define T_continue     263
#define T_default      264
#define T_do           265
#define T_DOWNTO       266
#define T_else         267
#define T_false        268
#define T_FOR          269
#define T_FORM         270
#define T_FUNC         271
#define T_if           272
#define T_int          273
#define T_MOD          274
#define T_return       275
#define T_STEP         276
#define T_switch       277
#define T_TO           278
#define T_true         279
#define T_while        280
#define T_WRITE        281
#define T_WRITELN      282
#define T_WRITESP      283
#define T_WRITESPLN    284

#define T_id           285
#define T_int_const    286
#define T_eq           287
#define T_neq          288
#define T_geq          289
#define T_leq          290
#define T_logicAnd     291
#define T_logicOr      292
#define T_plusplus     293
#define T_minusminus   294
#define T_pluseq       295
#define T_minuseq      296
#define T_multeq       297
#define T_diveq        298
#define T_modeq        299
#define T_float_const  300
#define T_char_const   301
#define T_string_const 302

int linecount = 1;
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
"MOD"           { return T_MOD      ;}
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
"&&" { return T_logicAnd; }
"||" { return T_logicOr; }
"++" { return T_plusplus; }
"--" { return T_minusminus; }
"+=" { return T_pluseq; }
"-=" { return T_minuseq; }
"*=" { return T_multeq; }
"/=" { return T_diveq; }
"%=" { return T_modeq; }

[ \t]    { /* empty */ }
\/\/.*   { /* empty */ } /* one line comments */

\'({PRINTABLE}|{ESCAPESEQ})\'  { return T_char_const; }

\"([^\\\"\n]|{ESCAPESEQ})*\"     { return T_string_const; }

"\/*"            { BEGIN(COMMENT); }
<COMMENT>"*\/"   { BEGIN(INITIAL); }
<COMMENT>\n      { linecount++;    }
<COMMENT>\*      { /* empty */     }
<COMMENT>[^*\n]+ { /* empty */     }

\r   { /* empty */ }
\n   { linecount++; }

.        { fprintf(stderr, "[Lexical error]:%d unmatched character '%c'\n", linecount, yytext[0]); exit(1); }

<<EOF>> { return T_eof; }

%%

int main() {
   int t;

   while ( (t = yylex()) != T_eof) {
      printf("%d: token ID: %d, yytext: %s\n", linecount, t, yytext);
   }

   return 0;
}
