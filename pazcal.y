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
Type varType, arrayType;

int writeType; // 0 for write, 1 for writeln, etc...
bool firstWriteArgument;

SymbolEntry *func, *p;
Stack Func = NULL,     // SymbolEntry pointers to current function (the one being evaluated for calling)
      Param = NULL,    // SymbolEntry pointers to current function's parameter list
      Array = NULL,    // SymbolEntry pointers to current array
      Breaks = NULL,   // Next lists (used in backpatching) for jumps which correspond
                       //     to breaks (only inside loops for now) TODO: implement for switch as well
      Continues = NULL; // Next lists (used in backpatching) for jumps which correspong to continues
int openLoops = 0;
unsigned long long cannotBreak = 0; // Used as bitwise stack of boolean values
#define PUSH_LOOP   cannotBreak = (cannotBreak << 1); openLoops++
#define POP_LOOP    cannotBreak >>= 1; openLoops--
#define PUSH_SWITCH cannotBreak = (cannotBreak << 1) | 1
#define POP_SWITCH  cannotBreak >>= 1

bool insideRoutine = false; // Used for generating intermediate code
                            // for the begining/ending of a routine
%}

%union {
   RepChar    chr;
   RepInteger integer;
   RepReal    real;
   RepString  str;

   rlvalue    RLvalue;
   lvalue     Lvalue;
   Type       t;
   Range      RAnge;
   loopContext LoopContext;

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
%type<RLvalue>       opt_var_init
%type<cnst>          pub_opt_var_init
%type<RLvalue>       expr
%type<Lvalue>        l_value;
%type<chr>           assign
/*%type<integer>       more_l_value; */
%type<RLvalue>       call
%type<RLvalue>       opt_expr
%type<RLvalue>       opt_format_expr
%type<chr>           pm
%type<RLvalue>       stmt
%type<RLvalue>       block
%type<RLvalue>       opt_block
%type<RLvalue>       opt_step
%type<integer>       to_downto
%type<RAnge>         range

%type<integer>       M
%type<RLvalue>       N

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

/* TODO: For Intermediate code generation
         X Function call ImmC
        ~X For loop ImmC
         X do - while ImmC
         X break/continue for loops
         * Switch statement ImmC
         X Variable initialization ImmC
         * Reduce/Reduce Conflict resolution due to nonterminal N appearing before else
         X WRITE / READ ImmC
            X FORM
            X booleans
         * !! Massive Code Cleanup !!
         * Use midrule actions for context saving instead of ad hoc Stacks
         * Proper printing of ImmC
*/
%%

module : /* Empty */  | declaration module ;

declaration : const_def | pub_var_def | routine | program ;

const_def : "const" type T_id '=' const_expr { addConstant($3, $2, $5); }
            opt_const_def ';' ;
opt_const_def : /* Empty */
              | opt_const_def ',' T_id '=' const_expr
                { addConstant($3, $<t>-4, $5); } ;

pub_var_def : type { varType = $1; } pub_var_init pub_opt_var_def ';' ;
pub_opt_var_def : /* Empty */ | ',' pub_var_init pub_opt_var_def ;

pub_var_init : T_id pub_opt_var_init
               {
                  SymbolEntry *p = newVariable($1, varType);
                  SymbolEntry *q = addConstant(newConstName(), varType, $2);
                  genQuad(ASG, Var(q), EMT, Var(p));
               }
             | T_id '[' { arrayType = varType; } const_expr ']'
               { arrayType = arrayTypeCheck( $4, arrayType ); }
               array_var_init { newVariable($1, arrayType); };

pub_opt_var_init : /* Empty */ { $$ = (Const) { varType, {0} }; }
                 | '=' const_expr  { $$ = $2; };

var_def : type { varType = $1; } var_init opt_var_def ';' ;
opt_var_def : /* Empty */ | ',' var_init opt_var_def ;

var_init : T_id opt_var_init
           {
              SymbolEntry *p = newVariable($1, varType);
              rlvalue result;

              if (!equalType($2.t, typeVoid)) {
                 if (!assignmentCompatibleTypes(varType, $2.t))
                    error("incompatible types in initialization");

                 if (equalType($2.t, typeBoolean)) {
                    result = genCodeBooleanExpr($2, p);
                    backpatch(result.Next, nextQuad()+1);
                 } else
                    result = $2;
                 genQuad(ASG, Var(result.Place), EMT, Var(p));
              }
            }
         | T_id '['
           { arrayType = varType;
             /*TODO: Array types should be created right to left if possible */
           } 
           const_expr ']' { arrayType = arrayTypeCheck( $4, arrayType ); }
           array_var_init { newVariable($1, arrayType); };

opt_var_init : /* Empty */ { $$.t = typeVoid; }
             | '=' expr  { $$ = $2; };
array_var_init : /* Empty */
               | '[' const_expr ']'
                  { arrayType = arrayTypeCheck( $2, arrayType ); }
                  array_var_init ;

routine_header : proc_func T_id
                 /* TODO: somehow many statements of the form
                          function_call($i) have been replaced by
                          function_call$i   probably by some s/../../g
                          Fortunately Bison transforms $i to (...) so
                          the final code is correct but I should fix it soon
                  */
                 { func = newFunction($2); openScope(); insideRoutine = true; }
                 '(' opt_args ')'
                 { endFunctionHeader(func, $1);
                   #ifdef DEBUG_SYMBOL
                     printf("Beginning function body:\n"); printSymbolTable();
                   #endif
                 } ;
proc_func : "PROC" { $$ = typeVoid; } | "FUNC" type { $$ = $2; };
parameter : type { varType = $1; } formal ;
opt_args  : /* Empty */  |     parameter more_args ;
more_args : /* Empty */  | ',' parameter more_args ;

formal : T_id     { newParameter($1, varType, PASS_BY_VALUE, func); }
       | '&' T_id { newParameter($2, varType, PASS_BY_REFERENCE, func); }
       | T_id '[' { arrayType = varType; } opt_const_expr ']' 
         {
            if ( equalType($4.type, typeVoid) )
               arrayType = typeIArray(arrayType);
            else
               arrayType = arrayTypeCheck( $4, arrayType );
         }
         array_formal
         { newParameter($1, arrayType, PASS_BY_REFERENCE, func); };
opt_const_expr : /* Empty */ { $$ = (Const) { typeVoid, {0} }; }
               | const_expr  { $$ = $1; } ;
array_formal : /* Empty */
             | '[' const_expr ']'
               { arrayType = arrayTypeCheck( $2, arrayType ); } array_formal ;

routine : routine_header ';' { forwardFunction(func); closeScope(); }
        | routine_header block
          {
            closeScope();
            backpatch($2.Next, nextQuad());
            genQuad(ENDU, Var(func), EMT, EMT);
          } ;

program_header : "PROGRAM" T_id '(' ')'
                 {
                    func = newFunction($2);
                    endFunctionHeader(func, typeVoid);
                    genQuad(UNIT, Var(func), EMT, EMT);
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
             backpatch($3.Next, nextQuad());
             genQuad(ENDU, Var(func), EMT, EMT);
          };

type : "int"  { $$ = typeInteger; }
     | "bool" { $$ = typeBoolean; }
     | "char" { $$ = typeChar; }
     | "REAL" { $$ = typeReal; } ;

const_expr : T_int_const        { $$ = (Const) { typeInteger, {.vInteger=$1} }; }
           | T_float_const      { $$ = (Const) { typeReal, {.vReal=$1} }; }
           | T_char_const       { $$ = (Const) { typeChar, {.vChar=$1} }; }
           | T_string_literal   { $$ = (Const) { typeArray(strlen($1), typeChar), {.vString=$1} }; }
           | "true"             { $$ = (Const) { typeBoolean, {.vBoolean=true} }; }
           | "false"            { $$ = (Const) { typeBoolean, {.vBoolean=false} }; }
           | '(' const_expr ')' { $$ = $2; }
           | T_id
             {
               p = lookupEntry($1, LOOKUP_ALL_SCOPES, true);
               if (p->entryType != ENTRY_CONSTANT) {
                  error("identifier '%s' is not a constant", $1);
                  $$.type = typeVoid; // Used as a recovery strategy.
               } else
                  $$ = p->u.eConstant;
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

expr : T_int_const         { $$.t = typeInteger; $$.Place = newConstant(newConstName(), typeInteger, $1); }
     | T_float_const       { $$.t = typeReal;    $$.Place = newConstant(newConstName(), typeReal,    $1); }
     | T_char_const        { $$.t = typeChar;    $$.Place = newConstant(newConstName(), typeChar,    $1); }
     | T_string_literal    { $$.t = typeArray(strlen($1), typeChar); $$.Place = newConstant(newConstName(), $$.t, $1); }
     | "true"
       {
          $$ = (rlvalue) { TRUE, typeBoolean };
          $$.True = makeList(nextQuad());
          genQuad(JUMP, EMT, EMT, EMT);
       }
     | "false"
       {
          $$ = (rlvalue) { FALSE, typeBoolean };
          $$.False = makeList(nextQuad());
          genQuad(JUMP, EMT, EMT, EMT);
       }
     | '(' expr ')'        { $$ = $2; }
     | l_value
       {
          $$.t = $1.type;
          $$.Place = findLvaluePlace($1);

          if (equalType($$.t, typeBoolean)) {
             $$.True = makeList(nextQuad());
             genQuad(IFB, Var($$.Place), EMT, EMT);
             $$.False = makeList(nextQuad());
             genQuad(JUMP, EMT, EMT, EMT);
          }
       }
     | call                { $$ = $1; }
     | '+' expr %prec UNOP { $$ = unopCodeGen('+', $2); }
     | '-' expr %prec UNOP { $$ = unopCodeGen('-', $2); }
     | '!' expr %prec UNOP
      {
         $$.t = unopTypeCheck('!', $2.t);

         $$.True = $2.False;
         $$.False = $2.True;
      }
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
     | expr "and" M expr
      {
         $$.t = exprTypeCheck('&', $1.t, $4.t);

         backpatch($1.True, $3);
         $$.True = $4.True;
         $$.False = mergeLists($1.False, $4.False);
      }
     | expr "or" M expr
      {
         $$.t = exprTypeCheck('|', $1.t, $4.t);

         backpatch($1.False, $3);
         $$.True = mergeLists($1.True, $4.True);
         $$.False = $4.False;
      }

M : /* empty */ { $$ = nextQuad(); }
N : /* empty */ { $$.Next = makeList(nextQuad()); genQuad(JUMP, EMT, EMT, EMT); }

l_value : T_id
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

            $$ = (lvalue) { ZERO, p, t };
          }
        | l_value '[' expr ']'
          {
            if ($1.type->kind != TYPE_ARRAY && $1.type->kind != TYPE_IARRAY)
               error(" [ expr ] applied on something not an array");

            $$.array = $1.array;
            $$.type  = $1.type->refType;

            SymbolEntry *t1, *t2;
            t1 = newConstant(newConstName(), typeInteger, $1.type->size);
            t2 = newTemporary(typeInteger);
            $$.addr = newTemporary(typeInteger);
            genQuad('*', Var( $1.addr ), Var(t1), Var(t2) );
            genQuad('+', Var(t2), Var( $3.Place ), Var( $$.addr ) );
          }
        ;

     /* TODO: Continue Code clean up from here */
call : T_id '(' 
         {
            p = lookupEntry($1, LOOKUP_ALL_SCOPES, true);
            if (p->entryType != ENTRY_FUNCTION)
               error("object \"%s\" is not callable", $1);
            Func = pushSymEntry(Func, p);
            Param = pushSymEntry(Param, p->u.eFunction.firstArgument);
            #ifdef DEBUG_SYMBOL
               //warning("Pushing param pointer %d for function \"%s\"", Param->p, p->id);
            #endif
         }
      opt_call ')'
         {
            if ( top(Param) != NULL )
               error("Function \"%s\" needs more arguments", ((SymbolEntry *) top(Func))->id);

            $$.t = ((SymbolEntry *) top(Func))->u.eFunction.resultType;
            if (!equalType($$.t, typeVoid)) {
               SymbolEntry *t = newTemporary($$.t);
               $$.Place = t;

               genQuad(PAR, Var(t), Mode(PASS_RET), EMT);

            }
            genQuad(CALL, EMT, EMT, Var((SymbolEntry *) top(Func)));

            if (equalType($$.t, typeBoolean)) {
               $$.True = makeList(nextQuad());
               genQuad(IFB, Var($$.Place), EMT, EMT);
               $$.False = makeList(nextQuad());
               genQuad(JUMP, EMT, EMT, EMT);
            }

            Func = pop(Func);
            Param = pop(Param);
         };

opt_call      : /* Empty */
              | expr { Param = paramCodeGen(Func, Param, $1); } more_opt_call ;
more_opt_call : /* Empty */
              | ',' expr { Param = paramCodeGen(Func, Param, $2); } more_opt_call ;

block : '{'
         {
           openScope();
           if (insideRoutine)
              genQuad(UNIT, Var(func), EMT, EMT);
            insideRoutine = false;
         }
         opt_block '}'
         {
            $$ = $3;
            closeScope();
         } ;
opt_block : /* Empty */
          {
             /* Use .t to signify the end of a block */
             $$.t = NULL;
             $$.Next = NULL;
          }
          | local_def opt_block { $$ = $2; }
          | stmt M opt_block
            {
               // TODO: A little Ad hoc so it might be incomplete..
               $$.Next = $3.Next;
               if ($3.t == NULL)
                  $$.Next = $1.Next;
               else
                  backpatch($1.Next, $2);

               $$.t = typeVoid;
            };

local_def : const_def | var_def ;

stmt : ';' { $$.Next = NULL; }
      | l_value assign expr ';'
         {
            if (!assignmentCompatibleTypes($1.type, $3.t)) { error("type mismatch on assignment"); printMismatch($1.type, $3.t); }
            if ($2 != '=' && !arithmeticType($1.type)) error("this assignment operator only works on arithmetic types");

            p = findLvaluePlace($1);

            if (equalType($3.t, typeBoolean))
               $$ = genCodeBooleanExpr($3, p);
            else {
               if ($2 != '=')
                  genQuad($2, Var(p), Var($3.Place), Var(p));
               else
                  genQuad(ASG, Var($3.Place), EMT, Var(p));
            }
         }
      | l_value pm ';'
      {
         if (!arithmeticType($1.type))
            error("increment/decrement operators work only on arithmetic types");
         p = findLvaluePlace($1);
         genQuad($2, Var(p), Cnst(1), Var(p));

         $$.Next = NULL;
      }
      | call ';' { if (!equalType($1.t, typeVoid)) warning("ignoring function result"); }
      | "if" '(' expr ')' M stmt
         {
            if (!equalType($3.t, typeBoolean))
               error("condition of the 'if' statement is not a boolean");

            backpatch($3.True, $5);
            $$.Next = mergeLists($3.False, $6.Next);
         }
      | "if" '(' expr ')' M stmt N "else" M stmt
         {
            if (!equalType($3.t, typeBoolean))
               error("condition of the 'if-else' statement is not a boolean");

            backpatch($3.True, $5);
            backpatch($3.False, $9);
            labelListType *tmp = mergeLists($6.Next, $10.Next);
            $$.Next = mergeLists(tmp, $7.Next);
         }
      | "while" M '(' expr ')' M
         {
            PUSH_LOOP;
            if (!equalType($4.t, typeBoolean))
               error("condition of the 'while' statement is not a boolean");

            Breaks = pushList(Breaks, (labelListType *) NULL);
            //Continues = pushLabel(Continues, $2);
            Continues = pushList(Continues, (labelListType *) NULL);
         }
         stmt
         {
            POP_LOOP;

            backpatch($4.True, $6);
            backpatch($8.Next, $2);
            $$.Next = $4.False;
            genQuad(JUMP, EMT, EMT, (opts) { LBL, (contentType) { .label = $2 } });

            $$.Next = mergeLists($$.Next, (labelListType *) top(Breaks));
            Breaks = pop(Breaks);

            backpatch((labelListType *) top(Continues), $2);
            Continues = pop(Continues);
         }
      | "FOR" '(' T_id ','
         {
            p = lookupEntry($3, LOOKUP_ALL_SCOPES, true);
            if (p->entryType == ENTRY_FUNCTION) error("identifier not a variable");
            else if ( !( (p->entryType == ENTRY_CONSTANT && equalType(p->u.eConstant.type, typeInteger) )
                   || (p->entryType == ENTRY_VARIABLE && equalType(p->u.eVariable.type, typeInteger) )
                   || (p->entryType == ENTRY_PARAMETER && equalType(p->u.eParameter.type, typeInteger) ) ) )
               error("control variable in FOR statement is not an integer");

            Breaks = pushList(Breaks, (labelListType *) NULL);
            Continues = pushList(Continues, (labelListType *) NULL);
         }
         range ')'
         {
            PUSH_LOOP;

            SymbolEntry *t1 = $6.from.Place; // Holds the FROM part
            SymbolEntry *t2 = $6.to.Place; // Holds the TO part
            SymbolEntry *t3 = $6.step.Place; // Holds the STEP
            SymbolEntry *t4 = newTemporary(typeInteger); // Holds the current value of the control variable
            labelListType *Next = NULL;
            p = lookupEntry($3, LOOKUP_ALL_SCOPES, true); // Symboltable entry for control variable

            if ($6.direction == -1) // Negative step
               genQuad('-', Cnst(0), Var(t3), Var(t3));

            genQuad(ASG, Var(t1), EMT, Var(t4));

            int LoopBeg = nextQuad();
            genQuad(ASG, Var(t4), EMT, Var(p));
            if ($6.direction == 1)
               genQuad(',', Var(p), Var(t2), Lbl(nextQuad() + 1));
            else
               genQuad('.', Var(p), Var(t2), Lbl(nextQuad() + 1));

            Next = makeList( nextQuad() );
            genQuad(JUMP, EMT, EMT, EMT);

            $<LoopContext>$ = (loopContext) { .t1 = t1, .t2 = t2, .t3 = t3, .t4 = t4, .Next = Next, .LoopBeg = LoopBeg };
         }
         stmt
         {
            p = lookupEntry($3, LOOKUP_ALL_SCOPES, true); // Symboltable entry for control variable

            backpatch($9.Next, nextQuad());

            $$.Next = mergeLists( $<LoopContext>8.Next, (labelListType *) top( Breaks ) );
            Breaks = pop(Breaks);

            backpatch( (labelListType *) top(Continues), nextQuad() );
            Continues = pop(Continues);

            if ($6.direction == 1) genQuad('+', Var($<LoopContext>8.t4), Var($<LoopContext>8.t3), Var($<LoopContext>8.t4));
            else                   genQuad('-', Var($<LoopContext>8.t4), Var($<LoopContext>8.t3), Var($<LoopContext>8.t4));

            genQuad(JUMP, EMT, EMT, Lbl($<LoopContext>8.LoopBeg));

            POP_LOOP;
         }
      | "do" M
         {
            PUSH_LOOP;
            Breaks = pushList(Breaks, (labelListType *) NULL);
            Continues = pushList(Continues, (labelListType *) NULL);
         } stmt "while" M '(' expr ')' ';'
         {
            backpatch($8.True, $2);
            backpatch($4.Next, $6);
            $$.Next = $8.False;
            genQuad(JUMP, EMT, EMT, (opts) { LBL, (contentType) { .label = $6 } });

            if (!equalType($8.t, typeBoolean))
               error("condition of the 'do-while' statement is not a boolean");

            POP_LOOP;
            $$.Next = mergeLists($$.Next, (labelListType *) top(Breaks));
            Breaks = pop(Breaks);

            labelListType *p = (labelListType *) top(Continues);
            Continues = pop(Continues);
            backpatch(p, $6);
         }
      | "switch" '(' expr ')'
         {
            PUSH_SWITCH;
            if (!equalType($3.t, typeInteger))
               error("switch expression is not an integer");
         }
         '{' opt_case_clause opt_default_clause '}' { POP_SWITCH; cannotBreak >>= 1; }
      | "break" ';'
         {
            if ((cannotBreak & 1)) error("break cannot be used in this context");
            
            labelListType *Next = (labelListType *) top(Breaks);
            Breaks = pop(Breaks);
            Next = mergeLists(Next, makeList(nextQuad()));
            Breaks = pushList(Breaks, Next);
            genQuad(JUMP, EMT, EMT, EMT);
         }
      | "continue" ';'
         {
            if (!openLoops) error("continue used outside of a loop");

            labelListType *Next = (labelListType *) top(Continues);
            Continues = pop(Continues);
            Next = mergeLists(Next, makeList(nextQuad()));
            Continues = pushList(Continues, Next);
            genQuad(JUMP, EMT, EMT, EMT);

            //int *p = (int *) top(Continues);
            //genQuad(JUMP, EMT, EMT, (opts) { LBL, (contentType) { .label = *p } });
         }
      | "return" opt_expr ';'
      {
         rlvalue result;

         if (!compatibleTypes($2.t, func->u.eFunction.resultType))
            error("Incompatible types in return statement");

         if (!equalType(func->u.eFunction.resultType, typeVoid)) {
            SymbolEntry *tmp = newTemporary(func->u.eFunction.resultType);
            if (equalType($2.t, typeBoolean)) {
               result = genCodeBooleanExpr($2, tmp);
               backpatch(result.Next, nextQuad());
            } else
               result = $2;

            genQuad(RETV, Var(result.Place), EMT, EMT); // TODO: replace RETV with $$
            /*
            if (equalType(result.t, typeVoid))
               genQuad(RET, EMT, EMT, EMT);
            else
               genQuad(RETV, Var(result.Place), EMT, EMT); // TODO: replace RETV with $$
            */
         } else genQuad(RET, EMT, EMT, EMT);
            
      }
      | write { firstWriteArgument = true; } '(' opt_format ')'
         {
            if (writeType % 2 == 1) { // Should print a new line
               genQuad(PAR, Var(NEWLINE), Mode(PASS_BY_VALUE), EMT);
               genQuad(PAR, Cnst(1),      Mode(PASS_BY_VALUE), EMT);
               genQuad(CALL, EMT, EMT, Var(lookupEntry("WRITE_CHAR", LOOKUP_ALL_SCOPES, true)));
            }
         } ';'
      | block { $$ = $1; };
pm                : "++" { $$ = '+'; } | "--" { $$ = '-'; };
opt_case_clause   : /* Empty */
                  | "case" const_expr
                     {
                        if (!equalType($2.type, typeInteger))
                           error("Case expression is not an integer");
                     }
                  ':' more_case clause opt_case_clause;
more_case         : /* Empty */
                  | "case" const_expr
                     {
                        if (!equalType($2.type, typeInteger))
                           error("Case expression is not an integer");
                     }
                  ':' more_case ;
opt_default_clause: /* Empty */ | "default" ':' clause ;
opt_expr          : /* Empty */ { $$.t = typeVoid; }
                  | expr        { $$ = $1; };
opt_format        : /* Empty */ | format more_format ;
more_format       : /* Empty */ | ',' format more_format ;

assign : '='  { $$ = '='; }
       | "+=" { $$ = '+'; }
       | "-=" { $$ = '-'; }
       | "*=" { $$ = '*'; }
       | "/=" { $$ = '/'; }
       | "%=" { $$ = '%'; };

range : expr
      {
         if (!equalType($1.t, typeInteger) )
            error("range expression in FOR statement is not an integer");
      }
      to_downto expr
      {
         if (!equalType($4.t, typeInteger) )
            error("range expression in FOR statement is not an integer");
      }
      opt_step
      { $$ = (Range) { .from = $1, .to = $4, .direction = $3, .step = $6 }; };
to_downto : "TO" { $$ = 1; } | "DOWNTO" { $$ = -1; };
opt_step : /* Empty */
         {
            SymbolEntry *tmp = newTemporary(typeInteger);
            genQuad(ASG, Cnst(1), EMT, Var(tmp));
            $$ = (rlvalue) { .t = typeInteger, .Place = tmp };
         }
         | "STEP" expr 
         {
            if (!equalType($2.t, typeInteger) )
               error("step expression in FOR statement is not an integer");

            //TODO: add check for negative step
            $$ = $2;
         };

clause : more_clause opt_clause ;
more_clause : /* Empty */ %prec SWITCH_BRK | stmt more_clause ;
opt_clause : "break" ';' | "NEXT" ';' ;

write : "WRITE"     { writeType = 0; }
      | "WRITELN"   { writeType = 1; }
      | "WRITESP"   { writeType = 2; }
      | "WRITESPLN" { writeType = 3; } ;

format : expr
         {
            genCodeWrite($1, firstWriteArgument, false, writeType, NULL, NULL);
            firstWriteArgument = false;
         }
       | "FORM" '(' expr
         {
            if (!arithmeticType($3.t) && !equalType($3.t, typeBoolean) && !equalType($3.t, typeIArray(typeChar)))
               error("non basic type (or string) given to a write command");
         }
       ',' expr
         {
            if (!equalType($6.t, typeInteger))
               error("format's second parameter (field length) is not a integer");
         }
       opt_format_expr
         {
            if (!equalType($8.t, typeVoid)) {
               if (!equalType($8.t, typeInteger))
                  error("format's third parameter (decimal digits) is not an integer");
               if (!equalType($3.t, typeReal))
                  error("expression of type REAL was expected since FORM has three parameters");
            }

            genCodeWrite($3, firstWriteArgument, true, writeType, $6, $8);

            firstWriteArgument = false;
         }
       ')' ;
opt_format_expr : /* Empty */ { $$.t = typeVoid; } | ',' expr { $$ = $2; } ;

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

   return 0;
}
