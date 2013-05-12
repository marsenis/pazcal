#include <stdio.h>
#include "general.h"
#include "semantics.h"

/* Stacks */
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
   else if ( !compatibleTypes(t->u.eParameter.type, expr)) {
   //else if ( !( (equalType(expr, typeInteger) || equalType(expr, typeChar) || equalType(expr, typeReal) ) && (equalType(t->u.eParameter.type, typeInteger) || equalType(t->u.eParameter.type, typeChar) || equalType(t->u.eParameter.type, typeReal) ) ) && !equalType(t->u.eParameter.type, expr) ) {
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

char aritheticType(Type t) {
   return equalType(t, typeInteger) || equalType(t, typeReal) || equalType(t, typeChar);
}

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

int numOp(char op) { return op == '+' || op == '-' || op == '*' || op == '/' || op == '%'; }
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

Type arrayTypeCheck(Const c, Type arrayType) {
   if ( equalType( c.t, typeChar) ) {
      if ( c.v.chr < 0 ) error("negative array size");
      return typeArray( c.v.chr, arrayType );
   } else if ( equalType( c.t, typeInteger) ) {
      if ( c.v.integer < 0 ) error("negative array size");
      return typeArray( c.v.integer, arrayType );
   }
   else
      error("array size not an integer"); // TODO: better error message for multiple dimensions
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
