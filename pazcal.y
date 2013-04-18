%{
#include <stdio.h>

#include "error.h"
%}

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
%token T_MOD          "MOD"
%token T_NEXT         "NEXT"
%token T_not          "not"
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

%token T_id
%token T_int_const
%token T_eq           "=="
%token T_neq          "!="
%token T_geq          ">="
%token T_leq          "<="
%token T_logicAnd     "&&"
%token T_logicOr      "||"
%token T_plusplus     "++"
%token T_minusminus   "--"
%token T_pluseq       "+="
%token T_minuseq      "-="
%token T_multeq       "*="
%token T_diveq        "/="
%token T_modeq        "%="
%token T_float_const
%token T_char_const
%token T_string_literal

/*%right "else"
%left "break"*/
%nonassoc '=' "++" "--" "+=" "-=" "*=" "/=" "%="

%left '!' "not"
%left '*' '/' '%' "MOD"
%left '+' '-'
%left '<' '>' "<=" ">="
%left "==" "!="
%left "&&" "and"
%left "||" "or"

   /* %left UMINUS UPLUS */

%%

module : /* Empty */  | declaration module ;

declaration : const_def | var_def | routine | program ;

const_def : "const" type T_id '=' const_expr opt_const_def ';' ;
opt_const_def : /* Empty */ | ',' T_id '=' const_expr opt_const_def ;

var_def : type var_init opt_var_def ';' ;
opt_var_def : /* Empty */ | ',' var_init opt_var_def ;

var_init : T_id opt_var_init | T_id '[' const_expr ']' array_var_init ;
opt_var_init : /* Empty */ | '=' expr ;
array_var_init : /* Empty */ | '[' const_expr ']' array_var_init ;

routine_header : proc_func T_id '(' opt_args ')' ;
proc_func : "PROC" | "FUNC" type ;
opt_args : /* Empty */ | type formal more_args ;
more_args : /* Empty */  | ',' type formal more_args ;

formal : T_id | '&' T_id | T_id '[' opt_const_expr ']' array_formal
opt_const_expr : /* Empty */ | const_expr ;
array_formal : /* Empty */ | '[' const_expr ']' array_formal ;

routine : routine_header more_routine ;
more_routine : ';' | block ;

program_header : "PROGRAM" T_id '(' ')' ;

program : program_header block ;

type : "int" | "bool" | "char" | "REAL" ;

const_expr : expr ;

   /* TODO: should make flex consider '%' and 'MOD' as equal tokens as well as '&&' "AND" '||' "OR" */
expr : T_int_const | T_float_const | T_char_const | T_string_literal
     | "true" | "false" | '(' expr ')' | l_value | call /* unop expr */
     | expr '+' expr | expr '-' expr | expr '*' expr | expr '/' expr
     | expr '%' expr | expr "MOD" expr | expr "==" expr | expr "!=" expr
     | expr '<' expr | expr '>' expr | expr "<=" expr | expr ">=" expr
     | expr "&&" expr | expr "and" expr | expr "||" expr | expr "or" expr ;

l_value : T_id more_l_value ;
more_l_value : /* Empty */ | '[' expr ']' more_l_value;

   /* TODO: Add rules/precedence for unop */
   /* unop : '+' %prec UPLUS | '-' %prec UMINUS | '!' | "not" ; */

call : T_id '(' opt_call ')' ;
opt_call : /* Empty */ | expr more_opt_call
more_opt_call : /* Empty */ | ',' expr more_opt_call ;

block : '{' opt_block '}' ;
opt_block : /* Empty */ | local_def opt_block | stmt opt_block ;

local_def : const_def | var_def ;

   /* dangling-else shift/reduce conflict : automaticaly resolved correctly */
   /* TODO: break inside case is NOT resolved correctly. It produces 3 shift/reduce conflicts */
stmt : ';' | l_value assign expr ';' | l_value pm ';' | call ';'
     | "if" '(' expr ')' stmt | "if" '(' expr ')' stmt "else" stmt | "while" '(' expr ')' stmt
     | "FOR" '(' T_id ',' range ')' stmt | "do" stmt "while" '(' expr ')' ';'
     | "switch" '(' expr ')' '{' opt_case opt_default '}'
     | "break" ';' | "continue" ';' | "return" opt_expr ';'
     | write '(' opt_format ')' ';' | block;
pm : "++" | "--" ;
opt_case : /* Empty */ | "case" const_expr ':' more_case clause ;
more_case : /* Empty */ | "case" const_expr ':' more_case ;
opt_default : "default" ':' clause ;
opt_expr : /* Empty */ | ',' expr ;
opt_format : /* Empty */ | format more_format;
more_format : /* Empty */ | ',' format more_format ;

assign : '=' | "+=" | "-=" | "*=" | "/=" | "%=" ;

range : expr to_downto expr opt_step ;
to_downto : "TO" | "DOWNTO" ;
opt_step : /* Empty */ | "STEP" expr ;

clause : more_clause opt_clause ;
more_clause : /* Empty */ | stmt more_clause ;
opt_clause : "break" ';' | "NEXT" ';' ;

write : "WRITE" | "WRITELN" | "WRITESP" | "WRITESPLN" ;

format : expr | "FORM" '(' expr ',' expr opt_format_expr ')' ;
opt_format_expr : /* Empty */ | ',' expr ;

%%

int main() { return yyparse(); }
