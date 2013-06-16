#include <stdio.h>
#include "general.h"
#include "semantics.h"

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

SymbolEntry* top(Stack S) {
   if (S == NULL) internal("SymbolEntry Stack is empty");
   return S->p;
}

Stack push(Stack S, SymbolEntry* p) {
   Stack t = (Stack) new(sizeof(struct StackTag));

   t->p = p;
   t->next = S;
   return t;
}

Stack paramCheck(Stack Func, Stack Param, Type expr) {
   SymbolEntry *f = top(Func);
   SymbolEntry *t = top(Param);

   if (t == NULL)
      error("Function \"%s\" needs less arguments", f->id);
   else if ( !assignmentCompatibleTypes(t->u.eParameter.type, expr)) {
      error("Type missmatch on the parameters given to the function \"%s\"", f->id);
#ifdef DEBUG_SYMBOL
      printf("Type missmatch ");
      printType(t->u.eParameter.type);
      printf(", ");
      printType(expr);
      printf(" on the parameters given to the function \"%s\"\n", f->id);
#endif
   }
   
   Param = pop(Param);
   return push(Param, t->u.eParameter.next);
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
         //return t1;
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
 * defined on page 11, line 15 for which the function assignmentCompatibleTypes
 * is the appropriate one
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
            error("incompatible type in unary operator '+'");
         return c1;
      case '-':
         if (equalType(c1.type, typeInteger))
            return (Const) { typeInteger, { (RepInteger) (- c1.value.vInteger) } };
         else if (equalType(c1.type, typeChar))
            return (Const) { typeChar, { (RepChar) (- c1.value.vChar) } };
         else if (equalType(c1.type, typeReal))
            return (Const) { typeReal, { (RepReal) (- c1.value.vReal) } };
         else
            error("incompatible type in unary operator '-'");
         break;
      case '!':
         if (!equalType(c1.type, typeBoolean))
            error("incompatible type in unary operator 'not'");
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

void addConstant(char *name, Type t, Const c) {
   Const cp;

   if (!assignmentCompatibleTypes(t, c.type))
      error("incompatible types in assignment");
   else
      cp = promote(c, t);

   if (equalType(t, typeBoolean))
       newConstant(name, t, cp.value.vBoolean);
   else if (equalType(t, typeInteger))
       newConstant(name, t, cp.value.vInteger);
   else if (equalType(t, typeChar))
       newConstant(name, t, cp.value.vChar);
   else if (equalType(t, typeReal))
       newConstant(name, t, cp.value.vReal);
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

// TODO: use only variable inside contentType instead of variable/constant for easy access
opts makeOperantFromPlace(SymbolEntry *place) {
   opts p;
   switch (place->entryType) {
      case ENTRY_VARIABLE: case ENTRY_TEMPORARY: case ENTRY_PARAMETER:
         p = (opts) {VAR, (contentType) { .variable = place } };
         break;
      case ENTRY_CONSTANT:
         p = (opts) {CONST, (contentType) { .constant = place } };
         break;
   }
   return p;
}

nonterm exprCodeGen(char op, nonterm t1, nonterm t2) {
   nonterm result;
   SymbolEntry *tmp;

   result.t = exprTypeCheck(op, t1.t, t2.t);

   opts op1 = makeOperantFromPlace(t1.Place);
   opts op2 = makeOperantFromPlace(t2.Place);

   switch (op) {
      case '+': case '-': case '*': case '/': case '%':
         tmp = newTemporary(result.t);
         genQuad(op, op1, op2, (opts) {VAR, (contentType) { .variable = tmp } } );
         result.Place = tmp;
         break;
   }

   return result;
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
}
