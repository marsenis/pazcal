#include <stdio.h>
#include "general.h"
#include "semantics.h"

#define EMT ((opts) { EMPTY, 0 })  // Empty Quad
//#define Var(X) ( (opts) { VAR, (contentType) { .variable = (X) } } )

/* Stacks used for maintaining inheritted attributes
 * in the SDT schemas. Specificaly, the stacks can hold
 * SymbolEntry pointers.
**/
Stack pop(Stack S) {
   Stack t;
   t = S->next;
   delete(S);
   return t;
}

void* top(Stack S) {
   if (S == NULL) internal("SymbolEntry Stack is empty");
   switch (S->type) {
      case SYM_ENTRY:
         return (void*)S->u.p;
      case NEXT_LIST:
         return (void*)S->u.l;
   }
}

Stack pushSymEntry(Stack S, SymbolEntry *p) {
   Stack t = (Stack) new(sizeof(struct StackTag));

   t->type = SYM_ENTRY;
   t->u.p = p;

   t->next = S;
   return t;
}

Stack pushList(Stack S, labelListType *p) {
   Stack t = (Stack) new(sizeof(struct StackTag));

   t->type = NEXT_LIST;
   t->u.l = p;

   t->next = S;
   return t;
}

Stack paramCodeGen(Stack Func, Stack Param, rlvalue expr) {
   SymbolEntry *f = (SymbolEntry *) top(Func);
   SymbolEntry *t = (SymbolEntry *) top(Param);

   if (t == NULL)
      error("Function \"%s\" needs less arguments", f->id);
   else if ( !assignmentCompatibleTypes(t->u.eParameter.type, expr.t)) {
      error("Type missmatch on the parameters given to the function \"%s\"", f->id);
#ifdef DEBUG_SYMBOL
      printf("Type missmatch ");
      printType(t->u.eParameter.type);
      printf(", ");
      printType(expr.t);
      printf(" on the parameters given to the function \"%s\"\n", f->id);
#endif
   }
   
   rlvalue result;
   if (equalType(expr.t, typeBoolean)) {
      SymbolEntry *tmp = newTemporary(typeBoolean);
      result = genCodeBooleanExpr(expr, tmp);
      backpatch(result.Next, nextQuad());
   } else
      result = expr;

   genQuad(PAR, Var(result.Place), Mode(t->u.eParameter.mode), EMT);
   Param = pop(Param);
   return pushSymEntry(Param, t->u.eParameter.next);
}

RepTypes applyInteger(char op, RepInteger x, RepInteger y) {
   switch (op) {
      case '+': return (RepTypes) ( x + y  );
      case '-': return (RepTypes) ( x - y  );
      case '*': return (RepTypes) ( x * y  );
      case '/': return (RepTypes) ( x / y  );
      case '%': return (RepTypes) ( x % y  );
      case '<': return (RepTypes) ( x < y  );
      case '>': return (RepTypes) ( x > y  );
      case ',': return (RepTypes) ( x <= y );
      case '.': return (RepTypes) ( x >= y );
      case '=': return (RepTypes) ( x == y );
      case '!': return (RepTypes) ( x != y );
   }
   return (RepTypes) 0;
}

RepTypes applyChar(char op, RepChar x, RepChar y) {
   switch (op) {
      case '+': return (RepTypes) ( x + y  );
      case '-': return (RepTypes) ( x - y  );
      case '*': return (RepTypes) ( x * y  );
      case '/': return (RepTypes) ( x / y  );
      case '%': return (RepTypes) ( x % y  );
      case '<': return (RepTypes) ( x < y  );
      case '>': return (RepTypes) ( x > y  );
      case ',': return (RepTypes) ( x <= y );
      case '.': return (RepTypes) ( x >= y );
      case '=': return (RepTypes) ( x == y );
      case '!': return (RepTypes) ( x != y );
   }
   return (RepTypes) 0;
}

RepTypes applyReal(char op, RepReal x, RepReal y) {
   switch (op) {
      case '+': return (RepTypes) ( x + y );
      case '-': return (RepTypes) ( x - y );
      case '*': return (RepTypes) ( x * y );
      case '/': return (RepTypes) ( x / y );
      case '<': return (RepTypes) ( x < y );
      case '>': return (RepTypes) ( x > y );
      case ',': return (RepTypes) ( x <= y );
      case '.': return (RepTypes) ( x >= y );
      case '=': return (RepTypes) ( x == y );
      case '!': return (RepTypes) ( x != y );
   }
   return (RepTypes) 0;
}

RepTypes applyBoolean(char op, RepBoolean x, RepBoolean y) {
   switch (op) {
      case '=': return (RepTypes) ( x == y );
      case '!': return (RepTypes) ( x != y );
      case '&': return (RepTypes) ( x && y );
      case '|': return (RepTypes) ( x || y );
   }
   return (RepTypes) 0;
}

Const promote(Const c, Type t) {
   Const res;

   res.type = t;
   if (equalType(t, typeInteger)) {

      if (equalType(c.type, typeInteger))
         res.value.vInteger = c.value.vInteger;
      else if (equalType(c.type, typeChar))
         res.value.vInteger = (RepInteger) c.value.vChar;
      else if (equalType(c.type, typeReal))
         res.value.vInteger = (RepInteger) c.value.vReal;

   } else if (equalType(t, typeChar)) {

      if (equalType(c.type, typeInteger))
         res.value.vChar = (RepInteger) c.value.vInteger;
      else if (equalType(c.type, typeChar))
         res.value.vChar = c.value.vChar;
      else if (equalType(c.type, typeReal))
         res.value.vChar = (RepChar) c.value.vReal;

   } else if (equalType(t, typeReal)) {

      if (equalType(c.type, typeInteger))
         res.value.vReal = (RepReal) c.value.vInteger;
      else if (equalType(c.type, typeChar))
         res.value.vReal = (RepReal) c.value.vChar;
      else if (equalType(c.type, typeReal))
         res.value.vReal = c.value.vReal;
   } else
      return c;

   return res;
}

const char* show(char op) {
   switch (op) {
      case '+': return "+"  ;
      case '-': return "-"  ;
      case '*': return "*"  ;
      case '/': return "/"  ;
      case '%': return "%"  ;
      case '<': return "<"  ;
      case '>': return ">"  ;
      case ',': return "<=" ;
      case '.': return ">=" ;
      case '=': return "==" ;
      case '!': return "!=" ;
      case '&': return "and";
      case '|': return "or" ;
   }
   return "undefined";
}

Type generalType(Type t1, Type t2) {
   if (equalType(t1, typeReal) || equalType(t2, typeReal)) return typeReal;
   else if (equalType(t1, typeInteger) || equalType(t2, typeInteger)) return typeInteger;
   else if (equalType(t1, typeChar) || equalType(t2, typeChar)) return typeChar;
   else return typeVoid;
}

Type compatibleOperants(char op, Type t1) {
   switch(op) {
      case '&': case '|':
         if (!equalType(t1, typeBoolean))
            error("incompatible types of operants in operation '%s'", show(op));
         return t1;
      case '%':
         if (!equalType(t1, typeInteger) && !equalType(t1, typeChar))
            error("incompatible types of operants in operation '%s'", show(op));
         return typeInteger; // Page 10, line 16
      case '<': case '>': case ',': case '.': case '=': case '!':
         if (!arithmeticType(t1))
            error("incompatible types of operants in operation '%s'", show(op));
         return typeBoolean;
      // +, -, *, /
      default:
         if (!arithmeticType(t1))
            error("incompatible types of operants in operation '%s'", show(op));
         // Page 10, line 14
         if (equalType(t1, typeReal)) return typeReal;
         else return typeInteger;
         //return t1;
   }
   return typeVoid;
}

/* 'int', 'REAL' and 'char' are compatible with each other
 * because each one of them can be casted to any other
 * inside an expression.
 * ATTENTION: not to be confused with compatible types for assignment
 * defined on page 11, line 15 for which the function
 * assignmentCompatibleTypes is the appropriate one
 */
bool compatibleTypes(Type t1, Type t2) {
   return
   (
      (    equalType(t1, typeInteger)
        || equalType(t1, typeChar)
        || equalType(t1, typeReal) )
      &&
      (    equalType(t2, typeInteger)
        || equalType(t2, typeChar)
        || equalType(t2, typeReal) )
   ) || equalType(t1, t2);
}

// t1 := t2; Page 11, line 15
bool assignmentCompatibleTypes(Type t1, Type t2) {
   return equalType(t1, t2)
       || ( equalType(t2, typeInteger) && equalType(t1, typeReal) )
       || ( equalType(t2, typeChar) && equalType(t1, typeInteger) )
       || ( equalType(t2, typeInteger) && equalType(t1, typeChar) );
}

bool arithmeticType(Type t) {
   return equalType(t, typeInteger) || equalType(t, typeChar) || equalType(t, typeReal);
}

Const applyUnop(char op, Const c1) {
   // Page 10, lines 6-9
   switch (op) {
      case '+':
         if (!arithmeticType(c1.type))
            error("non arithmetic-type operant after unary '+'");
         return c1;
      case '-':
         if (equalType(c1.type, typeInteger))
            return (Const) { typeInteger, { (RepInteger) (- c1.value.vInteger) } };
         else if (equalType(c1.type, typeChar))
            return (Const) { typeChar, { (RepChar) (- c1.value.vChar) } };
         else if (equalType(c1.type, typeReal))
            return (Const) { typeReal, { (RepReal) (- c1.value.vReal) } };
         else
            error("non arithmetic-type operant after unary '-'");
         break;
      case '!':
         if (!equalType(c1.type, typeBoolean))
            error("non boolean-type operant after unary not/'!'");
         return (Const) { typeBoolean, { (RepBoolean) (! c1.value.vBoolean) } };
   }

   return (Const) { typeVoid, {0} };
}

Const applyOperation(char op, Const c1, Const c2) {
   Const cp1, cp2, result; // promoted constants

#ifdef DEBUG_SYMBOL
   printf("Handling operator '%s' with operants: ", show(op));
   printType(c1.type);
   printf(" ");
   printType(c2.type);
   printf("\n");
#endif

   // Type cheking and promotion as nessecary
   if (!compatibleTypes(c1.type, c2.type)) {
      error("incompatible types of operants in operation '%s'", show(op));
      return (Const) { typeVoid, {0} };
   } else if (equalType(c1.type, c2.type)) {
      cp1 = c1;
      cp2 = c2;
   } else {
      Type genType = generalType(c1.type, c2.type);
#ifdef DEBUG_SYMBOL
      printf("GenType = ");
      printType(genType);
      printf("\n");
#endif
      cp1 = promote(c1, genType);
      cp2 = promote(c2, genType);
   }

   result.type = compatibleOperants(op, cp1.type);
   if ( equalType(result.type, typeVoid) ) {
      error("incompatible types of operants in operation '%s'", show(op));
      return (Const) { typeVoid, {0} };
   }

#ifdef DEBUG_SYMBOL
   if (equalType(cp1.type, typeReal))
      warning("handling real constants %lf and %lf", cp1.value.vReal, cp2.value.vReal);
   else if (equalType(cp1.type, typeBoolean))
      warning("handling boolean constants %c and %c", cp1.value.vBoolean ? 't' : 'f', cp2.value.vBoolean ? 't' : 'f');
#endif

   if ( equalType(cp1.type, typeInteger) )
      result.value = applyInteger(op, cp1.value.vInteger, cp2.value.vInteger);
   else if ( equalType(cp1.type, typeChar) )
      result.value = applyChar(op, cp1.value.vChar, cp2.value.vChar);
   else if ( equalType(cp1.type, typeReal) )
      result.value = applyReal(op, cp1.value.vReal, cp2.value.vReal);
   else if ( equalType(cp1.type, typeBoolean) )
      result.value = applyBoolean(op, cp1.value.vBoolean, cp2.value.vBoolean);

#ifdef DEBUG_SYMBOL
   if (equalType(result.type, typeReal))
      warning("result = %lf", result.value.vReal);
   else if (equalType(result.type, typeBoolean))
      warning("result = %s", result.value.vBoolean ? "true" : "false");
#endif
   return result;

}

SymbolEntry* addConstant(char *name, Type t, Const c) {
   Const cp;

   if (!assignmentCompatibleTypes(t, c.type))
      error("incompatible types in assignment");
   else
      cp = promote(c, t);

   if (equalType(t, typeBoolean))
       return newConstant(name, t, cp.value.vBoolean);
   else if (equalType(t, typeInteger))
       return newConstant(name, t, cp.value.vInteger);
   else if (equalType(t, typeChar))
       return newConstant(name, t, cp.value.vChar);
   else if (equalType(t, typeReal))
       return newConstant(name, t, cp.value.vReal);
}

Type unopTypeCheck(char op, Type t) {
   if (op == '+' || op == '-') {
      //if (!equalType(t, typeInteger) && !equalType(t, typeChar) && !equalType(t, typeReal))
      if (!arithmeticType(t))
         error("non arithmetic-type operant after unary '%c'", op);
   } else if (op == '!') {
      if (!equalType(t, typeBoolean))
         error("non boolean-type operant after unary not/'!'");
   } else
      internal("unrecognized operator passed in unopTypeCheck");
   return t;
}  

Type exprTypeCheck(char op, Type t1, Type t2) {
   Type result;

   if (!compatibleTypes(t1, t2)) {
      error("incompatible types of operants in operation '%s'", show(op));
      return typeVoid;
   } else if (!equalType(t1, t2))
      t1 = t2 = generalType(t1, t2);

   result = compatibleOperants(op, t1);
   if ( equalType(result, typeVoid) ) {
      error("incompatible types of operants in operation '%s'", show(op));
      return typeVoid;
   }

   return result;
}

Type arrayTypeCheck(Const c, Type arrayType) {
   //TODO: maybe keep only the Integer case according
   //      to the language specification
   if ( equalType( c.type, typeChar) ) {
      if ( c.value.vChar < 0 ) error("negative array size");
      return typeArray( c.value.vChar, arrayType );
   } else if ( equalType( c.type, typeInteger) ) {
      if ( c.value.vInteger < 0 ) error("negative array size");
      return typeArray( c.value.vInteger, arrayType );
   }
   else
      error("array size not an integer"); // TODO: better error message for multiple dimensions
   return typeVoid;
}

rlvalue exprCodeGen(char op, rlvalue t1, rlvalue t2) {
   rlvalue result;

   result.t = exprTypeCheck(op, t1.t, t2.t);

   switch (op) {
      case '+': case '-': case '*': case '/': case '%':
         result.Place = newTemporary(result.t);
         genQuad(op, Var(t1.Place), Var(t2.Place), Var(result.Place));
         break;
      case '=': case '!': case '<': case '>': case ',': case '.':
         // Code for condition
         result.True = makeList(nextQuad());
         genQuad(op, Var(t1.Place), Var(t2.Place), EMT);
         result.False = makeList(nextQuad());
         genQuad(JUMP, EMT, EMT, EMT);
         break;
   }

   return result;
}

rlvalue unopCodeGen(char op, rlvalue t) {
   rlvalue result;

   result.t = unopTypeCheck(op, t.t);
   if (op == '-') {
      result.Place = newTemporary(result.t);
      genQuad('-', Cnst(0), Var(t.Place), Var(result.Place));
   } else if (op == '+')
      result = t;

   return result;
}

SymbolEntry *findLvaluePlace(lvalue l) {
   if (l.addr->entryType == ENTRY_CONSTANT)
      return l.array;
   else {
      SymbolEntry *p = newTemporary(typePointer(l.type));
      genQuad(ARRAY, Var(l.array), Var(l.addr), Var(p));
      return p;
   }
}

rlvalue genCodeBooleanExpr(rlvalue x, SymbolEntry *p) {
   rlvalue result;

   backpatch(x.True, nextQuad());
   genQuad(ASG, Cnst(1), EMT, Var(p));

   result.Next = makeList(nextQuad());
   genQuad(JUMP, EMT, EMT, EMT); 

   backpatch(x.False, nextQuad());
   genQuad(ASG, Cnst(0), EMT, Var(p));

   result.t = typeBoolean;
   result.Place = p;

   return result;
}

void genCodeWrite(rlvalue x, bool firstWriteArgument, bool format, int writeType, rlvalue w, rlvalue d) {
   if (!firstWriteArgument && writeType >= 2) { // Should put a space between the arguments printed
      genQuad(PAR, Var(SPACE), Mode(PASS_BY_VALUE), EMT);
      genQuad(PAR, Cnst(1),   Mode(PASS_BY_VALUE), EMT);
      genQuad(CALL, EMT, EMT, Var(lookupEntry("WRITE_CHAR", LOOKUP_ALL_SCOPES, true)));
   }

   if (!arithmeticType(x.t) && !equalType(x.t, typeBoolean) && !equalType(typeIArray(typeChar), x.t))
      error("non basic type (or string) given to a write command");
      
   if (equalType(x.t, typeBoolean)) {
      SymbolEntry *p = newTemporary(typeBoolean);
      rlvalue r = genCodeBooleanExpr(x, p);
      backpatch(r.Next, nextQuad());
      genQuad(PAR, Var(p), Mode(PASS_BY_VALUE), EMT);
   } else
      genQuad(PAR, Var(x.Place), Mode(PASS_BY_VALUE), EMT);

   if (format)
      genQuad(PAR, Var(w.Place), Mode(PASS_BY_VALUE), EMT);
   else
      genQuad(PAR, Cnst(1), Mode(PASS_BY_VALUE), EMT);

   if (equalType(x.t, typeInteger))
      genQuad(CALL, EMT, EMT, Var(lookupEntry("WRITE_INT", LOOKUP_ALL_SCOPES, true)));
   else if (equalType(x.t, typeChar))
      genQuad(CALL, EMT, EMT, Var(lookupEntry("WRITE_CHAR", LOOKUP_ALL_SCOPES, true)));
   else if (equalType(x.t, typeReal)) {
      if (format && !equalType(d.t, typeVoid))
         genQuad(PAR, Var(d.Place), Mode(PASS_BY_VALUE), EMT);
      else {
         SymbolEntry *q = newConstant(newConstName(), typeInteger, 7);
         genQuad(PAR, Var(q), Mode(PASS_BY_VALUE), EMT);
      }
      genQuad(CALL, EMT, EMT, Var(lookupEntry("WRITE_REAL", LOOKUP_ALL_SCOPES, true)));
   }
   else if (equalType(typeIArray(typeChar), x.t))
      genQuad(CALL, EMT, EMT, Var(lookupEntry("WRITE_STRING", LOOKUP_ALL_SCOPES, true)));
   else if (equalType(x.t, typeBoolean))
      genQuad(CALL, EMT, EMT, Var(lookupEntry("WRITE_BOOL", LOOKUP_ALL_SCOPES, true)));
}

void addLibraryFunctions() {
   SymbolEntry *p;

   // Input/Output Functions
   p = newFunction("putchar");
   openScope();

   newParameter("c", typeChar, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   p = newFunction("puts");
   openScope();
   newParameter("c", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   p = newFunction("WRITE_INT");
   openScope();
   newParameter("n", typeInteger, PASS_BY_VALUE, p);
   newParameter("w", typeInteger, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   p = newFunction("WRITE_BOOL");
   openScope();
   newParameter("b", typeBoolean, PASS_BY_VALUE, p);
   newParameter("w", typeInteger, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   p = newFunction("WRITE_CHAR");
   openScope();
   newParameter("c", typeChar, PASS_BY_VALUE, p);
   newParameter("w", typeInteger, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   p = newFunction("WRITE_REAL");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   newParameter("w", typeInteger, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   p = newFunction("WRITE_STRING");
   openScope();
   newParameter("s", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   newParameter("w", typeInteger, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   p = newFunction("READ_INT");
   endFunctionHeader(p, typeInteger);

   p = newFunction("READ_BOOL");
   endFunctionHeader(p, typeBoolean);

   p = newFunction("getchar");
   endFunctionHeader(p, typeChar);

   p = newFunction("READ_REAL");
   endFunctionHeader(p, typeReal);

   p = newFunction("READ_STRING");
   openScope();
   newParameter("size", typeInteger, PASS_BY_VALUE, p);
   newParameter("s", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   // Mathematical Functions
   p = newFunction("abs");
   openScope();
   newParameter("n", typeInteger, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeInteger);

   p = newFunction("fabs");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("sqrt");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("sin");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("cos");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("tan");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("arctan");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("exp");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("ln");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("pi");
   endFunctionHeader(p, typeReal);

   // Convertion Functions
   p = newFunction("trunc");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("round");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeReal);

   p = newFunction("TRUNC");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeInteger);

   p = newFunction("ROUND");
   openScope();
   newParameter("r", typeReal, PASS_BY_VALUE, p);
   closeScope();
   endFunctionHeader(p, typeInteger);

   // String functions
   p = newFunction("strlen");
   openScope();
   newParameter("s", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   closeScope();
   endFunctionHeader(p, typeInteger);

   p = newFunction("strcmp");
   openScope();
   newParameter("s1", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   newParameter("s2", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   closeScope();
   endFunctionHeader(p, typeInteger);

   p = newFunction("strcpy");
   openScope();
   newParameter("trg", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   newParameter("src", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   p = newFunction("strcat");
   openScope();
   newParameter("trg", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   newParameter("src", typeIArray(typeChar), PASS_BY_REFERENCE, p);
   closeScope();
   endFunctionHeader(p, typeVoid);

   newConstant("$SPACE", typeChar, ' ');
   newConstant("$NEWLINE", typeChar, ' ');
   newConstant("$ZERO", typeInteger, 0);
}
