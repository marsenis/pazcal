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
SymbolEntry *func, *p;

/* TODO: this definition is currently in
   symbol.h. Should be moved or use something
   else instead
   TODO: consider using lvalue struct from
   alan implementation
*/
/*
typedef struct {
   Type t;
   union {
      RepInteger integer;
      RepBoolean boolean;
      RepChar    chr;
      RepReal    real;
      RepString  str;
   } v;
} Const;
*/

%}

%union {
   RepChar    chr;
   RepInteger integer;
   RepReal    real;
   RepString  str;

   Type       t;

   Const      cnst;
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
%type<cnst> const_expr

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

const_def : "const" type T_id '=' const_expr
            { constType = $2;
              if ( !equalType($2, ($5).t) )
                 error("incompatible types in assignment");
              newConstant($3, $2, ($5).v);
            } opt_const_def ';' ;
opt_const_def : /* Empty */ | ',' T_id '=' const_expr
               { if ( !equalType(constType, ($4).t) )
                    error("incompatible types in assignment");
                 newConstant($2, constType, ($4).v);
               }
               opt_const_def ;

var_def : type { varType = $1; } var_init opt_var_def ';' ;
opt_var_def : /* Empty */ | ',' var_init opt_var_def ;

/* TODO: multidimensional arrays are temporarily not supported.
         Should fix this if required.
*/
var_init : T_id opt_var_init
           { newVariable($1, varType); }
         | T_id '[' const_expr ']'
           {
              if ( !equalType( ($3).t, typeInteger ) )
                 error("array size not an integer");
              newVariable($1, typeArray( ($3).v.integer, varType));
           } ;
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
formal : T_id { newParameter($1, varType, PASS_BY_VALUE, func); }
       | '&' T_id { newParameter($2, varType, PASS_BY_REFERENCE, func); }
       | T_id '[' ']' { newParameter($1, typeIArray(varType), PASS_BY_REFERENCE, func); } 
       | T_id '[' const_expr ']'
         {
            if ( !equalType( ($3).t, typeInteger ) )
               error("array size not an integer");
            newParameter($1, typeArray( ($3).v.integer, varType), PASS_BY_REFERENCE, func);
         } /* array_formal */ ;
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

const_expr : T_int_const { ($$).t = typeInteger; ($$).v.integer = $1; }
           | T_float_const { ($$).t = typeReal; ($$).v.real = $1; }
           | T_char_const { ($$).t = typeChar; ($$).v.chr = $1; }
           /* | T_string_literal { ($$).t = Type; ($$).v.str = $1; } */
           | "true" { ($$).t = typeBoolean; ($$).v.boolean = 1; }
           | "false" { ($$).t = typeBoolean; ($$).v.boolean = 0; }
           | '(' const_expr ')' { $$ = $2; }
           | T_id
             { p = lookupEntry($1, LOOKUP_ALL_SCOPES, true);
               if (p->entryType != ENTRY_CONSTANT)
                  error("identifier '%s' is not a constant", $1);
               ($$).t = p->u.eConstant.type;
                if ( equalType( ($$).t, typeInteger ) )
                   ($$).v.integer = p->u.eConstant.value.vInteger;
                else if ( equalType( ($$).t, typeReal ) )
                   ($$).v.real = p->u.eConstant.value.vReal;
                else if ( equalType( ($$).t, typeBoolean ) )
                   ($$).v.boolean = p->u.eConstant.value.vBoolean;
                else if ( equalType( ($$).t, typeChar ) )
                   ($$).v.chr = p->u.eConstant.value.vChar;
             }
               
           /* TODO: gather up all this code inside a function */
           /* TODO: support Chars and Reals (and maybe strings) */
           | '+' const_expr %prec UNOP
             {
               if ( !equalType( ($2).t, typeInteger ) && !equalType( ($2).t, typeReal) )
                  error("Unary operator '+' used with operand of"
                        " incompatible type");
               $$ = $2;
             }
           | '-' const_expr %prec UNOP
             {
               if ( !equalType( ($2).t, typeInteger ) && !equalType( ($2).t, typeReal) )
                  error("Unary operator '-' used with operand of"
                        " incompatible type");
               ($$).t = ($2).t;
               ($$).v.integer = - ($2).v.integer;
             }
           | const_expr '+' const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '+' used with operands of"
                        " incompatible type");
               ($$).t = typeInteger;
               ($$).v.integer = ($1).v.integer + ($3).v.integer;
             }
           | const_expr '-' const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '-' used with operands of"
                        " incompatible type");
               ($$).t = typeInteger;
               ($$).v.integer = ($1).v.integer - ($3).v.integer;
             }
           | const_expr '*' const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '*' used with operands of"
                        " incompatible type");
               ($$).t = typeInteger;
               ($$).v.integer = ($1).v.integer * ($3).v.integer;
             }
           | const_expr '/' const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '/' used with operands of"
                        " incompatible type");
               ($$).t = typeInteger;
               ($$).v.integer = ($1).v.integer / ($3).v.integer;
             }
           | const_expr '%' const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '\%' used with operands of"
                        " incompatible type");
               ($$).t = typeInteger;
               ($$).v.integer = ($1).v.integer % ($3).v.integer;
             }
           | const_expr "==" const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '==' used with operands of"
                        " incompatible type");
               ($$).t = typeInteger;
               ($$).v.boolean = ($1).v.integer == ($3).v.integer;
             }
           | const_expr "!=" const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '!=' used with operands of"
                        " incompatible type");
               ($$).t = typeInteger;
               ($$).v.boolean = ($1).v.integer != ($3).v.integer;
             }
           | const_expr '<' const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '<' used with operands of"
                        " incompatible type");
               ($$).t = typeBoolean;
               ($$).v.boolean = ($1).v.integer < ($3).v.integer;
             }
           | const_expr '>' const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '<' used with operands of"
                        " incompatible type");
               ($$).t = typeBoolean;
               ($$).v.boolean = ($1).v.integer > ($3).v.integer;
             }
           | const_expr "<=" const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '<=' used with operands of"
                        " incompatible type");
               ($$).t = typeBoolean;
               ($$).v.boolean = ($1).v.integer <= ($3).v.integer;
             }
           | const_expr ">=" const_expr
             {
               if ( !equalType( ($1).t, typeInteger ) || !equalType( ($3).t, typeInteger) )
                  error("Binary operator '>=' used with operands of"
                        " incompatible type");
               ($$).t = typeBoolean;
               ($$).v.boolean = ($1).v.integer >= ($3).v.integer;
             }
           | const_expr "and" const_expr
             {
               if ( !equalType( ($1).t, typeBoolean ) || !equalType( ($3).t, typeBoolean ) )
                  error("Binary operator 'and' used with operands of"
                        " incompatible type");
               ($$).t = typeBoolean;
               ($$).v.boolean = ($1).v.boolean && ($3).v.boolean;
             }
           | const_expr "or" const_expr
             {
               if ( !equalType( ($1).t, typeBoolean ) || !equalType( ($3).t, typeBoolean) )
                  error("Binary operator 'or' used with operands of"
                        " incompatible type");
               ($$).t = typeBoolean;
               ($$).v.boolean = ($1).v.boolean || ($3).v.boolean;
             }
           ;

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
