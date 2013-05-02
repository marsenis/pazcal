%{
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "general.h"
#include "symbol.h"
#include "pazcal.lex.h"

#define SYMB_TABLE_SIZE 10007

/* Type of a constant used in subrules of the grammar
   e.g. int a, b; because when the parser recognizes , b
   it won't be able to know it's type
*/
Type constType, varType;
SymbolEntry *func;
%}

%union {
   RepChar    chr;
   RepInteger integer;
   RepReal    real;
   RepString  str;

   Type       t;
}

%token T_and          "and" 
%token T_bool         "bool"
%token T_break        "break"
%token T_case         "case"
%token T_char         "char"
%token T_const        "const"
%token T_continue     "continue"
%token T_default      "default"
%token T_do           "do"
%token T_DOWNTO       "DOWNTO"
%token T_else         "else"
%token T_false        "false"
%token T_FOR          "FOR"
%token T_FORM         "FORM"
%token T_FUNC         "FUNC"
%token T_if           "if"
%token T_int          "int"
%token T_NEXT         "NEXT"
%token T_or           "or"
%token T_PROC         "PROC"
%token T_PROGRAM      "PROGRAM"
%token T_REAL         "REAL"
%token T_return       "return"
%token T_STEP         "STEP"
%token T_switch       "switch"
%token T_TO           "TO"
%token T_true         "true"
%token T_while        "while"
%token T_WRITE        "WRITE"
%token T_WRITELN      "WRITELN"
%token T_WRITESP      "WRITESP"
%token T_WRITESPLN    "WRITESPLN"

%token<str> T_id
%token T_eq           "=="
%token T_neq          "!="
%token T_geq          ">="
%token T_leq          "<="
%token T_plusplus     "++"
%token T_minusminus   "--"
%token T_pluseq       "+="
%token T_minuseq      "-="
%token T_multeq       "*="
%token T_diveq        "/="
%token T_modeq        "%="
%token<integer> T_int_const
%token<real>    T_float_const
%token<chr>     T_char_const
%token<str>     T_string_literal

%type<t> type
%type<t> proc_func

/* Assosiativity and precedence of operators */
/* %nonassoc '=' "++" "--" "+=" "-=" "*=" "/=" "%=" */

/* Fixes dangling else shift/reduce */
%nonassoc ')'
%nonassoc "else"

/* Fixes break inside switch shift/reduce conflict */
%nonassoc "break"
%nonassoc SWITCH_BRK

%left "or"
%left "and"
%left "==" "!="
%left '<' '>' "<=" ">="
%left '+' '-'
%left '*' '/' '%'
%left UNOP

/* TODO: For Semantic Analysis:
         * Type checking on expressions
         * break/continue only inside while/for/switch. (with special rule for switch)
         * function call type checking
         * array bounds checking
         * constants evaluation
         * add build-in functions to the outer scope
         ( * multidimensional arrays * )
*/
%%

module : /* Empty */  | declaration module ;

declaration : const_def | var_def | routine | program ;

/* TODO: add different production in <const_expr> other than <expr>
         in order to be able to statically evaluate constant expressions
         and bind their name with the value in the symbol table
*/
const_def : "const" type T_id '=' const_expr
            { constType = $2; newConstant($3, $2); } opt_const_def ';' ;
opt_const_def : /* Empty */ | ',' T_id '=' const_expr
               { newConstant($2, constType); }
               opt_const_def ;

var_def : type { varType = $1; } var_init opt_var_def ';' ;
opt_var_def : /* Empty */ | ',' var_init opt_var_def ;

/* TODO: multidimensional arrays are temporarily not supported.
         Should fix this if required.
*/
/* TODO: replace T_int_const with const_expr */
var_init : T_id opt_var_init
           { newVariable($1, varType); }
         | T_id '[' T_int_const ']'
           { newVariable($1, typeArray($3, varType)); } ;
opt_var_init : /* Empty */ | '=' expr ;
/* array_var_init : /* Empty | '[' const_expr ']' array_var_init ; */

routine_header : proc_func T_id
                 { func = newFunction($2); openScope(); } '(' opt_args ')'
                 { endFunctionHeader(func, $1); } ;
proc_func : "PROC" { $$ = typeVoid; } | "FUNC" type { $$ = $2; };
parameter : type { varType = $1; } formal ;
opt_args : /* Empty */ | parameter more_args ;
more_args : /* Empty */  | ',' parameter more_args ;

/* TODO: add support for multidimensional arrays if needed */
/* TODO: add const_expr instead of T_id */
formal : T_id { newParameter($1, varType, PASS_BY_VALUE, func); }
       | '&' T_id { newParameter($2, varType, PASS_BY_REFERENCE, func); }
       | T_id '[' ']' { newParameter($1, typeIArray(varType), PASS_BY_REFERENCE, func); } 
       | T_id '[' T_int_const ']' { newParameter($1, typeArray($3, varType), PASS_BY_REFERENCE, func); } /* array_formal */ ;
/* opt_const_expr : /* Empty | const_expr ; */
/* array_formal : /* Empty | '[' const_expr ']' array_formal ; */

routine : routine_header ';' { forwardFunction(func); closeScope(); }
        | routine_header block { closeScope(); } ;

program_header : "PROGRAM" T_id '(' ')' ;

program : program_header block ;

type : "int"  { $$ = typeInteger; }
     | "bool" { $$ = typeBoolean; }
     | "char" { $$ = typeChar; }
     | "REAL" { $$ = typeReal; } ;

const_expr : expr ;

expr : T_int_const | T_float_const | T_char_const { note("found char const '%c'", $1); }
     | T_string_literal { note("found string literal: \"%s\"", $1); }
     | "true" | "false" | '(' expr ')' | l_value | call | unop expr %prec UNOP
     | expr '+' expr | expr '-' expr | expr '*' expr | expr '/' expr
     | expr '%' expr | expr "==" expr | expr "!=" expr
     | expr '<' expr | expr '>' expr | expr "<=" expr | expr ">=" expr
     | expr "and" expr | expr "or" expr ;

l_value : T_id more_l_value ;
more_l_value : /* Empty */ | '[' expr ']' more_l_value;

unop : '+' | '-' | '!' ;

call : T_id '(' opt_call ')' ;
opt_call : /* Empty */ | expr more_opt_call
more_opt_call : /* Empty */ | ',' expr more_opt_call ;

block : '{' { openScope(); } opt_block '}' { closeScope(); } ;
opt_block : /* Empty */ | local_def opt_block | stmt opt_block ;

local_def : const_def | var_def ;

stmt : ';' | l_value assign expr ';' | l_value pm ';' | call ';'
     | "if" '(' expr ')' stmt | "if" '(' expr ')' stmt "else" stmt | "while" '(' expr ')' stmt
     | "FOR" '(' T_id ',' range ')' stmt | "do" stmt "while" '(' expr ')' ';'
     | "switch" '(' expr ')' '{' opt_case_clause opt_default_clause '}'
     | "break" ';' | "continue" ';' | "return" opt_expr ';'
     | write '(' opt_format ')' ';' | block;
pm : "++" | "--" ;
opt_case_clause : /* Empty */ | "case" const_expr ':' more_case clause opt_case_clause;
more_case : /* Empty */ | "case" const_expr ':' more_case ;
opt_default_clause : /* Empty */ | "default" ':' clause ;
opt_expr : /* Empty */ | expr ;
opt_format : /* Empty */ | format more_format ;
more_format : /* Empty */ | ',' format more_format ;

assign : '=' | "+=" | "-=" | "*=" | "/=" | "%=" ;

range : expr to_downto expr opt_step ;
to_downto : "TO" | "DOWNTO" ;
opt_step : /* Empty */ | "STEP" expr ;

clause : more_clause opt_clause ;
more_clause : /* Empty */ %prec SWITCH_BRK | stmt more_clause ;
opt_clause : "break" ';' | "NEXT" ';' ;

write : "WRITE" | "WRITELN" | "WRITESP" | "WRITESPLN" ;

format : expr | "FORM" '(' expr ',' expr opt_format_expr ')' ;
opt_format_expr : /* Empty */ | ',' expr ;

%%

/* flags:
      -f          : Read from standard input
      filename    : Read from file "filename"
*/
int main(int argc, char *argv[]) {
   if (argc != 2) { printf("Usage: %s [-f | filename]\n", argv[0]); exit(1); }

   linecount = 1;
   if (strcmp(argv[1], "-f") == 0) filename = "stdin";
   else {
      filename = argv[1];
      FILE *f = fopen(filename, "r");

      if (!f) { fprintf(stderr, "No such file or directory\n"); exit(2); }
      
      yy_switch_to_buffer(yy_create_buffer(f, YY_BUF_SIZE));
   }

   initSymbolTable(SYMB_TABLE_SIZE);
   openScope(); // Global scope

   if (yyparse()) exit(1);

   closeScope();
   //printSymbolTable();

   return 0;
}
