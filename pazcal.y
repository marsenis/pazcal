%{
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "general.h"
#include "symbol.h"
#include "pazcal.lex.h"
#include "semantics.h"

#define SYMB_TABLE_SIZE 10007

/* Type of a constant used in subrules of the grammar
   e.g. int a, b; because when the parser recognizes , b
   it won't be able to know it's type
*/
Type constType, varType, arrayType;
SymbolEntry *func, *p;
Stack Func = NULL, Param = NULL;

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
%type<cnst> opt_const_expr
%type<t> expr
%type<t> l_value;
%type<integer> more_l_value;
%type<t> call

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
         # Type checking on expressions
         * Semantic Analysis on statements
         * break/continue only inside while/for/switch. (with special rule for switch)
         # function call type checking
         # array bounds checking
         ~ constants evaluation
         # add build-in functions to the outer scope
         # multidimensional arrays
*/
%%

module : /* Empty */  | declaration module ;

declaration : const_def | var_def | routine | program ;

const_def : "const" type T_id '=' const_expr
            { constType = $2; addConstant($3, constType, $5); }
            opt_const_def ';' ;
opt_const_def : /* Empty */ | ',' T_id '=' const_expr
               { addConstant($2, constType, $4); }
               opt_const_def ;

var_def : type { varType = $1; } var_init opt_var_def ';' ;
opt_var_def : /* Empty */ | ',' var_init opt_var_def ;

var_init : T_id opt_var_init
           { newVariable($1, varType); }
         | T_id '[' { arrayType = varType; } const_expr ']'
           { arrayType = arrayTypeCheck( $4, arrayType ); } array_var_init
           { newVariable($1, arrayType); };
opt_var_init : /* Empty */ | '=' expr ;
array_var_init : /* Empty */
               | '[' const_expr ']'
               { arrayType = arrayTypeCheck( $2, arrayType ); } array_var_init ;

routine_header : proc_func T_id
                 { func = newFunction($2); openScope(); } '(' opt_args ')'
                 { endFunctionHeader(func, $1);
#ifdef DEBUG_SYMBOL
                 printf("Begining function body:\n"); printSymbolTable();
#endif
                 } ;
proc_func : "PROC" { $$ = typeVoid; } | "FUNC" type { $$ = $2; };
parameter : type { varType = $1; } formal ;
opt_args : /* Empty */ | parameter more_args ;
more_args : /* Empty */  | ',' parameter more_args ;

formal : T_id { newParameter($1, varType, PASS_BY_VALUE, func); }
       | '&' T_id { newParameter($2, varType, PASS_BY_REFERENCE, func); }
       | T_id '[' { arrayType = varType; } opt_const_expr ']' 
       {
          if ( equalType(($4).t, typeVoid) ) arrayType = typeIArray(arrayType);
          else arrayType = arrayTypeCheck( $4, arrayType );
       } array_formal { newParameter($1, arrayType, PASS_BY_REFERENCE, func); };
opt_const_expr : /* Empty */ { $$ = (Const) { typeVoid, {0} }; } | const_expr { $$ = $1; } ;
array_formal : /* Empty */
             | '[' const_expr ']'
               { arrayType = arrayTypeCheck( $2, arrayType); } array_formal ;

routine : routine_header ';' { forwardFunction(func); closeScope(); }
        | routine_header block { closeScope(); } ;

program_header : "PROGRAM" T_id '(' ')' ;

program : program_header
         {
#ifdef DEBUG_SYMBOL
            printf("-------- MAIN PROGRAM ----------\n");
            printSymbolTable();
#endif
         }
            block ;

type : "int"  { $$ = typeInteger; }
     | "bool" { $$ = typeBoolean; }
     | "char" { $$ = typeChar; }
     | "REAL" { $$ = typeReal; } ;

const_expr : T_int_const { ($$).t = typeInteger; ($$).v.integer = $1; }
           | T_float_const { ($$).t = typeReal; ($$).v.real = $1; }
           | T_char_const { ($$).t = typeChar; ($$).v.chr = $1; }
           | T_string_literal { ($$).t = typeIArray(typeChar); ($$).v.str = $1; }
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
                else if ( equalType( ($$).t, typeIArray(typeChar) ) )
                   ($$).v.str = p->u.eConstant.value.vString;
             }
               
           /* TODO: support unary operators for Real and Chars and 'not' for Booleans */
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
           | const_expr '+' const_expr  { $$ = applyOperation('+', $1, $3); }
           | const_expr '-' const_expr  { $$ = applyOperation('-', $1, $3); }
           | const_expr '*' const_expr  { $$ = applyOperation('*', $1, $3); }
           | const_expr '/' const_expr  { $$ = applyOperation('/', $1, $3); }
           | const_expr '%' const_expr  { $$ = applyOperation('%', $1, $3); }
           | const_expr "==" const_expr { $$ = applyOperation('=', $1, $3); }
           | const_expr "!=" const_expr { $$ = applyOperation('!', $1, $3); }
           | const_expr '<' const_expr  { $$ = applyOperation('<', $1, $3); }
           | const_expr '>' const_expr  { $$ = applyOperation('>', $1, $3); }
           | const_expr "<=" const_expr { $$ = applyOperation(',', $1, $3); }
           | const_expr ">=" const_expr { $$ = applyOperation('.', $1, $3); }
           | const_expr "and" const_expr{ $$ = applyOperation('&', $1, $3); }
           | const_expr "or" const_expr { $$ = applyOperation('|', $1, $3); }
           ;

expr : T_int_const { $$ = typeInteger; }
     | T_float_const { $$ = typeReal; }
     | T_char_const { $$ = typeChar; }
     | T_string_literal { $$ = typeIArray(typeChar); }
     | "true" { $$ = typeBoolean; }
     | "false" { $$ = typeBoolean; }
     | '(' expr ')' { $$ = $2; }
     | l_value { $$ = $1; }
     | call { $$ = $1; }
     | '+' expr %prec UNOP { $$ = unopTypeCheck('+', $2); }
     | '-' expr %prec UNOP { $$ = unopTypeCheck('-', $2); }
     | '!' expr %prec UNOP { $$ = unopTypeCheck('!', $2); }
     | expr '+' expr       { $$ = exprTypeCheck('+', $1, $3); }
     | expr '-' expr       { $$ = exprTypeCheck('-', $1, $3); }
     | expr '*' expr       { $$ = exprTypeCheck('*', $1, $3); }
     | expr '/' expr       { $$ = exprTypeCheck('/', $1, $3); }
     | expr '%' expr       { $$ = exprTypeCheck('%', $1, $3); }
     | expr "==" expr      { $$ = exprTypeCheck('=', $1, $3); }
     | expr "!=" expr      { $$ = exprTypeCheck('!', $1, $3); }
     | expr '<' expr       { $$ = exprTypeCheck('<', $1, $3); }
     | expr '>' expr       { $$ = exprTypeCheck('>', $1, $3); }
     | expr "<=" expr      { $$ = exprTypeCheck(',', $1, $3); }
     | expr ">=" expr      { $$ = exprTypeCheck('.', $1, $3); }
     | expr "and" expr     { $$ = exprTypeCheck('&', $1, $3); }
     | expr "or" expr      { $$ = exprTypeCheck('|', $1, $3); } ;

l_value : T_id
          more_l_value 
         {
            Type t;
            p = lookupEntry($1, LOOKUP_ALL_SCOPES, true);
            if (p->entryType == ENTRY_CONSTANT)
               t = p->u.eConstant.type;
            else if (p->entryType == ENTRY_VARIABLE)
               t = p->u.eVariable.type;
            else if (p->entryType == ENTRY_PARAMETER)
               t = p->u.eParameter.type;
            else
               error("identifier \"%s\" is not a variable/constant", $1);

            int i;
            for (i = 0; i < $2; i++) {
               if (t->kind != TYPE_ARRAY && t->kind != TYPE_IARRAY) error("identifier \"%s\" is not a %d-dimensional array", $1, $2);
                  t = t->refType;
            }
            $$ = t;
         };
more_l_value : /* Empty */ { $$ = 0; }
             | '[' expr ']' more_l_value
             { if (!equalType($2, typeInteger) && !equalType($2, typeChar))
                  error("array subscript is not an int/char");
               $$ = $4 + 1;
             }

call : T_id '(' 
      {
         p = lookupEntry($1, LOOKUP_ALL_SCOPES, true);
         if (p->entryType != ENTRY_FUNCTION)
            error("object \"%s\" is not callable", $1);
         Func = push(Func, p);
         Param = push(Param, p->u.eFunction.firstArgument);
#ifdef DEBUG_SYMBOL
         warning("Pushing param pointer %d for function \"%s\"", Param->p, p->id);
#endif
      } opt_call ')'
      {
         if ( top(Param) != NULL )
            error("Function \"%s\" needs more arguments", (top(Func))->id);
         $$ = (top(Func))->u.eFunction.resultType;
         Func = pop(Func);
         Param = pop(Param);
      };

opt_call : /* Empty */
         | expr {
#ifdef DEBUG_SYMBOL
            warning("matched an argument");
#endif
            Param = paramCheck(Func, Param, $1); } more_opt_call ;
more_opt_call : /* Empty */
         | ',' expr {
#ifdef DEBUG_SYMBOL
            warning("matched an argument");
#endif
            Param = paramCheck(Func, Param, $2); } more_opt_call ;

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
   openScope(); // library scope
   addLibraryFunctions(); 
   openScope(); // Global scope

   if (yyparse()) exit(1);

   closeScope();
   closeScope();
   //printSymbolTable();

   return 0;
}
