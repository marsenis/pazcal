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

Const applyOperation(char, Const, Const);
void addConstant(char *, Type, Const);

Type exprTypeCheck(char, Type, Type);
Type unopTypeCheck(char, Type);

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
         ~ Type checking on expressions
         * break/continue only inside while/for/switch. (with special rule for switch)
         * function call type checking
         * array bounds checking
         # constants evaluation
         ~ add build-in functions to the outer scope
         * multidimensional arrays
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

/* TODO: multidimensional arrays are temporarily not supported.
         Should fix this if required.
*/
var_init : T_id opt_var_init
           { newVariable($1, varType); }
         | T_id '[' const_expr ']'
           {
              if ( equalType( ($3).t, typeChar) ) {
                 if ( ($3).v.chr < 0 ) error("negative array size");
                 newVariable($1, typeArray( ($3).v.chr, varType ) );
              } else if ( equalType( ($3).t, typeInteger) ) {
                 if ( ($3).v.integer < 0 ) error("negative array size");
                 newVariable($1, typeArray( ($3).v.integer, varType));
              }
              else
                 error("array size not an integer");
           } ;
opt_var_init : /* Empty */ | '=' expr ;
/* array_var_init : /* Empty | '[' const_expr ']' array_var_init ; */

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
                else if ( equalType( ($$).t, typeIArray(typeChar) ) );
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

   /* unop : '+' | '-' | '!' ; */

/* TODO: parameter matching and checking for functions */
call : T_id '(' opt_call ')'
      {
         p = lookupEntry($1, LOOKUP_ALL_SCOPES, true);
         if (p->entryType != ENTRY_FUNCTION)
            error("object \"%s\" is not callable", $1);
         $$ = p->u.eFunction.resultType;
      };
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

/** TODO: Move all this code for type checking in a separate file */
RepInteger applyInteger(char op, RepInteger x, RepInteger y) {
   switch (op) {
      case '+': return x + y;
      case '-': return x - y;
      case '*': return x * y;
      case '/': return x / y;
      case '%': return x % y;
      case '<': return x < y;
      case '>': return x > y;
      case ',': return x <= y;
      case '.': return x >= y;
      case '=': return x == y;
      case '!': return x != y;
   }
}

RepChar applyChar(char op, RepChar x, RepChar y) {
   switch (op) {
      case '+': return x + y;
      case '-': return x - y;
      case '*': return x * y;
      case '/': return x / y;
      case '%': return x % y;
      case '<': return x < y;
      case '>': return x > y;
      case ',': return x <= y;
      case '.': return x >= y;
      case '=': return x == y;
      case '!': return x != y;
   }
}

RepReal applyReal(char op, RepReal x, RepReal y) {
   switch (op) {
      case '+': return x + y;
      case '-': return x - y;
      case '*': return x * y;
      case '/': return x / y;
      case '<': return x < y;
      case '>': return x > y;
      case ',': return x <= y;
      case '.': return x >= y;
      case '=': return x == y;
      case '!': return x != y;
   }
}

RepBoolean applyBoolean(char op, RepBoolean x, RepBoolean y) {
   switch (op) {
      case '<': return x < y;
      case '>': return x > y;
      case ',': return x <= y;
      case '.': return x >= y;
      case '=': return x == y;
      case '!': return x != y;
      case '&': return x && y;
      case '|': return x || y;
   }
}

Const promote(Const c, Type t) {
   Const res;

   res.t = t;
   if (equalType(t, typeInteger) && equalType(c.t, typeInteger))
      res.v.integer = c.v.integer;
   else if (equalType(t, typeInteger) && equalType(c.t, typeReal))
      res.v.integer = c.v.real;
   else if (equalType(t, typeInteger) && equalType(c.t, typeChar))
      res.v.integer = c.v.chr;

   else if (equalType(t, typeReal) && equalType(c.t, typeInteger))
      res.v.real = c.v.integer;
   else if (equalType(t, typeReal) && equalType(c.t, typeReal))
      res.v.real = c.v.real;
   else if (equalType(t, typeReal) && equalType(c.t, typeChar))
      res.v.real = c.v.chr;

   else if (equalType(t, typeChar) && equalType(c.t, typeInteger))
      res.v.chr = c.v.integer;
   else if (equalType(t, typeChar) && equalType(c.t, typeReal))
      res.v.chr = c.v.real;
   else if (equalType(t, typeChar) && equalType(c.t, typeChar))
      res.v.chr = c.v.chr;

   return res;
}

Const applyOperation(char op, Const c1, Const c2) {
   Const cp1, cp2, result; // promoted constants

   // Promote types as nessecary
   switch (op) {
      case '&': case '|':
         if ( !equalType(c1.t, typeBoolean) || !equalType(c2.t, typeBoolean) )
            error("incompatible types in operation '%c%c'", op, op); // hackia
         cp1 = c1;
         cp2 = c2;
         break;
      case '%':
         if ( equalType(c1.t, typeReal) || equalType(c2.t, typeReal) )
            error("operator '\%' used with real operands");
      default:
         if ( equalType(c1.t, typeReal) || equalType(c2.t, typeReal) ) {
            cp1 = promote(c1, typeReal);
            cp2 = promote(c2, typeReal);
         } else if ( equalType(c1.t, typeInteger) || equalType(c2.t, typeInteger) ) {
            cp1 = promote(c1, typeInteger);
            cp2 = promote(c2, typeInteger);
         } else if ( equalType(c1.t, typeChar) || equalType(c2.t, typeChar) ) {
            cp1 = promote(c1, typeChar);
            cp2 = promote(c2, typeChar);
         } else
            error("incompatible types in operation '%c'", op); // TODO: better error message: fix op
         break;
      //case '+': case '-': case '*': case '/':
      //case '<': case '>': case '=': case '!': case ',': case '.':
   }

   if (op == '<' || op == '>' || op == '=' || op == '!' || op == ',' || op == '.')
      result.t = typeBoolean;
   else
      result.t = cp1.t;

   /* TODO: !!IMPORTANT!! assigning to wrong fields of a union. This should be fixed ASAP */
   switch (cp1.t->kind) {
      case TYPE_INTEGER:
         result.v.integer = applyInteger(op, cp1.v.integer, cp2.v.integer);
         break;
      case TYPE_REAL:
         switch (result.t->kind) {
            case TYPE_BOOLEAN:
               result.v.boolean = applyReal(op, cp1.v.real, cp2.v.real);
               break;
            case TYPE_REAL:
               result.v.real = applyReal(op, cp1.v.real, cp2.v.real);
               break;
         }
         break;
      case TYPE_CHAR:
         result.v.chr = applyChar(op, cp1.v.chr, cp2.v.chr);
         break;
      case TYPE_BOOLEAN:
         result.v.boolean = applyBoolean(op, cp1.v.boolean, cp2.v.boolean);
         break;
   }
   
   return result;
}

void addConstant(char *name, Type t, Const c) {
   if ( !equalType(t, typeBoolean) && !equalType( c.t, typeBoolean ) ) {
       switch (t->kind) {
          case TYPE_INTEGER:
             newConstant(name, t, promote(c, t).v.integer);
             break;
          case TYPE_REAL:
             newConstant(name, t, promote(c, t).v.real);
             break;
          case TYPE_CHAR:
             newConstant(name, t, promote(c, t).v.chr);
             break;
       }
   } else if ( equalType(t, c.t) )
      newConstant(name, t, c.v.boolean);
   else
      error("incompatible types in assignment (probably involving booleans)");
}

Type unopTypeCheck(char op, Type t) {
   if (op == '+' || op == '-') {
      if (!equalType(t, typeInteger) && !equalType(t, typeChar) && !equalType(t, typeReal))
         error("incompatible type in unary \'%c\' operator", op);
   } else if (op == '!') {
      if (!equalType(t, typeBoolean))
         error("incompatible type in unary \'%c\' operator", op);
   } else
      internal("unrecognized operator passed in unopTypeCheck");
   return t;
}  

inline int numOp(char op) { return op == '+' || op == '-' || op == '*' || op == '/'; }
Type exprTypeCheck(char op, Type t1, Type t2) {
   switch (op) {
      case '&': case '|':
         if ( !equalType(t1, typeBoolean) || !equalType(t2, typeBoolean) )
            error("incompatible types in operation '%c%c'", op, op); // hackia
         return typeBoolean;
         break;
      case '%':
         if ( equalType(t1, typeReal) || equalType(t2, typeReal) )
            error("operator '\%' used with real operands");
      default:
         if ( equalType(t1, typeReal) || equalType(t2, typeReal) )
            return numOp(op) ? typeReal : typeBoolean;
         else if ( equalType(t1, typeInteger) || equalType(t2, typeInteger) )
            return numOp(op) ? typeInteger : typeBoolean;
         else if ( equalType(t1, typeChar) || equalType(t2, typeChar) )
            return numOp(op) ? typeChar : typeBoolean;
         else
            error("incompatible types in operation '%c'", op); // TODO: better error message: fix op
         break;
   }
   return typeInteger; // Default action. TODO: change it
}

void addLibraryFunctions() {
   SymbolEntry *p;

   // TODO: add more library functions
   p = newFunction("READ_INT");
   endFunctionHeader(p, typeInteger);
}

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
