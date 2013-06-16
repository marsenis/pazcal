%{
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "general.h"
#include "symbol.h"
#include "pazcal.lex.h"
#include "semantics.h"
#include "intermediateCode.h"

#define SYMB_TABLE_SIZE 10007

/* Type of a constant used in subrules of the grammar
   e.g. int a, b; because when the parser recognizes , b
   it won't be able to know it's type
*/
Type constType, varType, arrayType;
SymbolEntry *func, *p;
Stack Func = NULL, Param = NULL;
int openLoops = 0;
unsigned long long cannotBreak = 0; // Used as bitwise stack of boolean values
#define PUSH_LOOP   cannotBreak = (cannotBreak << 1); openLoops++
#define POP_LOOP    cannotBreak >>= 1; openLoops--
#define PUSH_SWITCH cannotBreak = (cannotBreak << 1) | 1
#define POP_SWITCH  cannotBreak >>= 1

bool insideRoutine = false; // Used for generating intermediate code
                            // for the begining/ending of a routine
#define EMT ((opts) { EMPTY, 0 })  // Empty Quad

// TODO: Currently the following code is in intermediateCode.h
//       Consider moving it to a more appropriate place
/*
typedef struct {
   SymbolEntry   *Place;
   Type          t;
   labelListType *Next, *True, *False;
} nonterm ;
*/
%}

%union {
   RepChar    chr;
   RepInteger integer;
   RepReal    real;
   RepString  str;

   nonterm    rlvalue;
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

%token<str>           T_id
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

%type<t>             type
%type<t>             proc_func
%type<cnst>          const_expr
%type<cnst>          opt_const_expr
%type<rlvalue>       expr
%type<rlvalue>       l_value;
%type<integer>       assign
%type<integer>       more_l_value;
%type<t>             call
%type<t>             opt_expr
%type<t>             opt_format_expr 

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
         # Semantic Analysis on statements
         # break/continue only inside while/for/switch. (with special rule for switch)
         # function call type checking
         # array bounds checking
         # constants evaluation
         # add build-in functions to the outer scope
         # multidimensional arrays
         # CODE CLEANUP


         ** ALL DONE :) **

   TODO: For Intermediate code generation
         * For starters fix all grammar attributes that expect a Type and get an rlvalue instead.
           In it's present form (14-6-13) it doesn't even compile
         * Generate code (obviously) for everything

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
                 { func = newFunction($2); openScope(); insideRoutine = true; } '(' opt_args ')'
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
          if ( equalType(($4).type, typeVoid) ) arrayType = typeIArray(arrayType);
          else arrayType = arrayTypeCheck( $4, arrayType );
       } array_formal { newParameter($1, arrayType, PASS_BY_REFERENCE, func); };
opt_const_expr : /* Empty */ { $$ = (Const) { typeVoid, {0} }; } | const_expr { $$ = $1; } ;
array_formal : /* Empty */
             | '[' const_expr ']'
               { arrayType = arrayTypeCheck( $2, arrayType); } array_formal ;

routine : routine_header ';' { forwardFunction(func); closeScope(); }
        | routine_header block
          {
            closeScope();
            genQuad(ENDU, (opts) {VAR, (contentType) { .variable = func } }, EMT, EMT);
          } ;

program_header : "PROGRAM" T_id '(' ')'
               {
                  func =  newFunction($2);
                  endFunctionHeader(func, typeVoid);
                  genQuad(UNIT, (opts) {VAR, (contentType) { .variable = func } }, EMT, EMT);
               };

program : program_header
         {
#ifdef DEBUG_SYMBOL
            printf("-------- MAIN PROGRAM ----------\n");
            printSymbolTable();
#endif
         }
            block
         {
            genQuad(ENDU, (opts) {VAR, (contentType) { .variable = func } }, EMT, EMT);
         };

type : "int"  { $$ = typeInteger; }
     | "bool" { $$ = typeBoolean; }
     | "char" { $$ = typeChar; }
     | "REAL" { $$ = typeReal; } ;

const_expr : T_int_const        { ($$).type = typeInteger; ($$).value.vInteger = $1; }
           | T_float_const      { ($$).type = typeReal; ($$).value.vReal = $1; }
           | T_char_const       { ($$).type = typeChar; ($$).value.vChar = $1; }
           | T_string_literal   { ($$).type = typeArray(strlen($1), typeChar); ($$).value.vString = $1; }
           | "true"             { ($$).type = typeBoolean; ($$).value.vBoolean = true; }
           | "false"            { ($$).type = typeBoolean; ($$).value.vBoolean = false; }
           | '(' const_expr ')' { $$ = $2; }
           | T_id
             { p = lookupEntry($1, LOOKUP_ALL_SCOPES, true);
               if (p->entryType != ENTRY_CONSTANT) {
                  error("identifier '%s' is not a constant", $1);
                  ($$).type = typeVoid;
               } else
                  ($$) = p->u.eConstant;
             }
               
           | '+' const_expr %prec UNOP  { $$ = applyUnop('+', $2); }
           | '-' const_expr %prec UNOP  { $$ = applyUnop('-', $2); }
           | '!' const_expr %prec UNOP  { $$ = applyUnop('!', $2); }
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

expr : T_int_const         { ($$).t = typeInteger; ($$).Place = newConstant(newConstName(), typeInteger, $1); }
     | T_float_const       { ($$).t = typeReal;    ($$).Place = newConstant(newConstName(), typeReal,    $1); }
     | T_char_const        { ($$).t = typeChar;    ($$).Place = newConstant(newConstName(), typeChar,    $1); }
     | T_string_literal    { ($$).t = typeArray(strlen($1), typeChar); ($$).Place = newConstant(newConstName(), typeArray(strlen($1), typeChar), $1);}
     | "true"              { ($$).t = typeBoolean; ($$).Place = newConstant(newConstName(), typeBoolean,  true);}
     | "false"             { ($$).t = typeBoolean; ($$).Place = newConstant(newConstName(), typeBoolean, false);}
     | '(' expr ')'        { $$ = $2; } /* { ($$).t = ($2; ($$).Place = ($2).Place; } */
     | l_value             { $$ = $1; }
     | call                { ($$).t = $1; }
     | '+' expr %prec UNOP { ($$).t = unopTypeCheck('+', ($2).t); }
     | '-' expr %prec UNOP { ($$).t = unopTypeCheck('-', ($2).t); }
     | '!' expr %prec UNOP { ($$).t = unopTypeCheck('!', ($2).t); }
     | expr '+' expr       { $$ = exprCodeGen('+', $1, $3); }
     | expr '-' expr       { $$ = exprCodeGen('-', $1, $3); }
     | expr '*' expr       { $$ = exprCodeGen('*', $1, $3); }
     | expr '/' expr       { $$ = exprCodeGen('/', $1, $3); }
     | expr '%' expr       { $$ = exprCodeGen('%', $1, $3); }
     | expr "==" expr      { $$ = exprCodeGen('=', $1, $3); }
     | expr "!=" expr      { $$ = exprCodeGen('!', $1, $3); }
     | expr '<' expr       { $$ = exprCodeGen('<', $1, $3); }
     | expr '>' expr       { $$ = exprCodeGen('>', $1, $3); }
     | expr "<=" expr      { $$ = exprCodeGen(',', $1, $3); }
     | expr ">=" expr      { $$ = exprCodeGen('.', $1, $3); }
     | expr "and" expr     { $$ = exprCodeGen('&', $1, $3); }
     | expr "or" expr      { $$ = exprCodeGen('|', $1, $3); } ;

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

               Type wholeType = t;

               int i;
               for (i = 0; i < $2; i++) {
                  if (t->kind != TYPE_ARRAY && t->kind != TYPE_IARRAY) {
                     error("identifier \"%s\" is not a %d-dimensional array", $1, $2);
                     break;
                  }
                  t = t->refType;
               }
               ($$).t = t;
               
               t = wholeType;
               if ($2 != 0) {
                  SymbolEntry *tp = newConstant(newConstName(), typeInteger, (RepInteger) 42); // TODO: get list with array subscripts
                  SymbolEntry *tmp1 = newTemporary(typePointer(t->refType));
                  SymbolEntry *tmp2 = newTemporary(t->refType);

                  genQuad(ARRAY, (opts) { VAR, (contentType) { .variable = p } }, (opts) { CONST, (contentType) { .constant = tp } }, (opts) { VAR, (contentType) { .variable = tmp1 } } );
                  genQuad(ASG, (opts) {VAR, (contentType) { .variable = tmp1 }}, EMT, (opts) {VAR, (contentType) { .variable = tmp2 }});
                  ($$).Place = tmp2;
               }
               else
                  ($$).Place = p;

            };
more_l_value : /* Empty */ { $$ = 0; } // 0-dimensions (not an array)
             | '[' expr ']' more_l_value
                // Page 9, Line 18
                { if (!equalType(($2).t, typeInteger))
                     error("array subscript is not an int");
                  $$ = ($4) + 1;      // one more dimension
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
         }
      opt_call ')'
         {
            if ( top(Param) != NULL )
               error("Function \"%s\" needs more arguments", (top(Func))->id);
            $$ = (top(Func))->u.eFunction.resultType;
            Func = pop(Func);
            Param = pop(Param);
         };

opt_call      : /* Empty */
              | expr { Param = paramCheck(Func, Param, ($1).t); } more_opt_call ;
more_opt_call : /* Empty */
              | ',' expr { Param = paramCheck(Func, Param, ($2).t); } more_opt_call ;

block : '{'
         { openScope();
           if (insideRoutine)
              genQuad(UNIT, (opts) {VAR, (contentType) { .variable = func } }, EMT, EMT);
            insideRoutine = false;
         } opt_block '}' { closeScope(); } ;
opt_block : /* Empty */ | local_def opt_block | stmt opt_block ;

local_def : const_def | var_def ;

stmt : ';'
      | l_value assign expr ';'
         {
            if (!assignmentCompatibleTypes(($1).t, ($3).t)) { error("type mismatch on assignment"); printMismatch(($1).t, ($3).t); }
            if ($2 != 1 && !arithmeticType(($1).t)) error("this assignment operator only works on arithmetic types");
         }
      | l_value pm ';'
         { if (!arithmeticType(($1).t)) error("increment/decrement operators only work on arithmetic types"); }
      | call ';' { if (!equalType($1, typeVoid)) warning("ignoring function result"); }
      | "if" '(' expr ')'  stmt { if (!equalType(($3).t, typeBoolean)) error("condition of the 'if' statement is not a boolean"); }
      | "if" '(' expr ')' stmt "else" stmt
         { if (!equalType(($3).t, typeBoolean)) error("condition of the 'if-else' statement is not a boolean"); }
      | "while" '(' expr ')'
         {
            PUSH_LOOP;
            if (!equalType(($3).t, typeBoolean))
               error("condition of the 'while' statement is not a boolean");
         }
         stmt { POP_LOOP; }
      | "FOR" '(' T_id ','
         {
            p = lookupEntry($3, LOOKUP_ALL_SCOPES, true);
            if (p->entryType == ENTRY_FUNCTION) error("identifier not a variable");
            else if ( !( (p->entryType == ENTRY_CONSTANT && equalType(p->u.eConstant.type, typeInteger) )
                   || (p->entryType == ENTRY_VARIABLE && equalType(p->u.eVariable.type, typeInteger) )
                   || (p->entryType == ENTRY_PARAMETER && equalType(p->u.eParameter.type, typeInteger) ) ) )
               error("control variable in FOR statement is not an integer");
         }
         range ')' { PUSH_LOOP; } stmt { POP_LOOP; }
      | "do" { PUSH_LOOP; } stmt "while" '(' expr ')' ';'
         {
            if (!equalType(($6).t, typeBoolean))
               error("condition of the 'do-while' statement is not a boolean");
            POP_LOOP;
         }
      | "switch" '(' expr ')'
         {
            PUSH_SWITCH;
            if (!equalType(($3).t, typeInteger))
               error("switch expression is not an integer");
         }
         '{' opt_case_clause opt_default_clause '}' { POP_SWITCH; cannotBreak >>= 1; }
      | "break" ';'           { if ((cannotBreak & 1)) error("break cannot be used in this context"); }
      | "continue" ';'        { if (!openLoops) error("continue used outside of a loop"); }
      | "return" opt_expr ';' { if (!compatibleTypes($2, func->u.eFunction.resultType)) error("Incompatible types in return statement"); }
      | write '(' opt_format ')' ';'
      | block;
pm                : "++" | "--" ;
opt_case_clause   : /* Empty */
                  | "case" const_expr
                     {
                        if (!equalType(($2).type, typeInteger))
                           error("Case expression is not an integer");
                     }
                  ':' more_case clause opt_case_clause;
more_case         : /* Empty */
                  | "case" const_expr
                     {
                        if (!equalType(($2).type, typeInteger))
                           error("Case expression is not an integer");
                     }
                  ':' more_case ;
opt_default_clause: /* Empty */ | "default" ':' clause ;
opt_expr          : /* Empty */ { $$ = typeVoid; } | expr { $$ = ($1).t; };
opt_format        : /* Empty */ | format more_format ;
more_format       : /* Empty */ | ',' format more_format ;

assign : '='  { $$ = 1; }
       | "+=" { $$ = 0; }
       | "-=" { $$ = 0; }
       | "*=" { $$ = 0; }
       | "/=" { $$ = 0; }
       | "%=" { $$ = 0; };

range : expr
      {
         if (!equalType(($1).t, typeInteger) )
            error("range expression in FOR statement is not an integer");
      }
      to_downto expr
      {
         if (!equalType(($4).t, typeInteger) )
            error("range expression in FOR statement is not an integer");
      }
      opt_step ;
to_downto : "TO" | "DOWNTO" ;
opt_step : /* Empty */ | "STEP" expr 
         {
            if (!equalType(($2).t, typeInteger) )
               error("step expression in FOR statement is not an integer");
         };

clause : more_clause opt_clause ;
more_clause : /* Empty */ %prec SWITCH_BRK | stmt more_clause ;
opt_clause : "break" ';' | "NEXT" ';' ;

write : "WRITE" | "WRITELN" | "WRITESP" | "WRITESPLN" ;

format : expr
         {
            if (!arithmeticType(($1).t) && !equalType(($1).t, typeBoolean) && !equalType(typeIArray(typeChar), ($1).t))
               error("non basic type (or string) given to a write command");
         }
       | "FORM" '(' expr
         {
            if (!arithmeticType(($3).t) && !equalType(($3).t, typeBoolean) && !equalType(($3).t, typeIArray(typeChar)))
               error("non basic type (or string) given to a write command");
         }
       ',' expr
         {
            if (!equalType(($6).t, typeInteger))
               error("format's second parameter (field length) is not a integer");
         }
       opt_format_expr
         {
            if (!equalType($8, typeInteger))
               error("format's third parameter (decimal digits) is not an integer");
            if (!equalType(($3).t, typeReal))
               error("expression of type REAL was expected since FORM has three parameters");
         }
       ')' ;
opt_format_expr : /* Empty */ { $$ = typeVoid; } | ',' expr { $$ = ($2).t; } ;

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

   printImm();

   closeScope();
   closeScope();
   //printSymbolTable();

   return 0;
}
