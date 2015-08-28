#include <string.h>
#include "target.h"
#include "intermediateCode.h"
#include "symbol.h"
#include "general.h"
#include "error.h"

#define gen(...) fprintf(asmfile, __VA_ARGS__)

int StringID = 0;
int newStringID() { return StringID++; }

// INT:     32-bits (4 bytes)
// CHAR:    8-bits  (1 byte)
// BOOL:    8-bits  (1 byte)
// POINTER: 64-bits (8 bytes)
// REAL:    64-bits (8 bytes)
#define DATATYPES 5
enum dataTypes { INT, CHAR, BOOL, POINTER, REAL };
char cmdModifiers[] = { 'l', 'b', 'b', 'q', 'd' };

#define REGS 10
enum regs { AX, BX, CX, DX, SI, DI, BP, SP, R8, R9 };
char regName[DATATYPES][REGS][5] =
{
   { "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp", "%r8d", "%r9d" },
   {  "%al",  "%bl",  "%cl",  "%dl", "%sil", "%dil", "%bpl", "%spl", "%r8b", "%r9b" },
   {  "%al",  "%bl",  "%cl",  "%dl", "%sil", "%dil", "%bpl", "%spl", "%r8b", "%r9b" },
   { "%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%rbp", "%rsp",  "%r8", "%r9"  }
};
enum regs args[6] = { DI, SI, DX, CX, R8, R9 };
//int revArgs[REGS] = {-1, -1, 3, 2, 1, 0, -1, -1, 4, 5 };
//SymbolEntry *argsSymEntry[6];  /* Symbol Table entries for the first 6 arguments */

SymbolEntry *currentFunc = NULL;;

void label(int i) {
   gen(".L%d:\n", i);
}

/* Translates a high level type to a low level dataType */
enum dataTypes trans(Type t) {
   if (equalType(t, typeInteger))
      return INT;
   else if (equalType(t, typeChar))
      return CHAR;
   else if (equalType(t, typeBoolean))
      return BOOL;
   else if (t->kind == TYPE_ARRAY || t->kind == TYPE_IARRAY)
      return POINTER;
   else if (equalType(t, typeReal))
      return REAL;
   else {
      internal("\rtarget.c:[trans]: invalid data type");
      return POINTER;
   }
}

char *reg(enum regs R, enum dataTypes t) {
   return regName[t][R];
}

char cmd(enum dataTypes t) {
   return cmdModifiers[t];
}

char *arithmCmd(enum opType op) {
   switch (op) {
      case '+': return "add";
      case '-': return "sub";
      case '*': return "imul";
      default: return "";
   }
}

char *compCmd(enum opType op) {
   switch (op) {
      case '=': return "e";
      case '<': return "l";
      case ',': return "le";
      case '>': return "g";
      case '.': return "ge";
      case '!': return "ne";
      default:
         internal("\rtarget.c:[compCmd]: Invalid comparison operator");
         return "";
   }
}

Type getVarType(opts x) {
   switch (x.type) {
      case CONST: return typeInteger;
      case VAR:   return getSymType(x.content.variable);
      default:
         internal("[target.c]:getVarSize: invalid opts arguments");
   }
   return NULL;
}

void TG_Preamble() {
   gen("\t.file\t\"%s\"\n", filename);
   //gen("\t.section\t.rodata\n");
}

void TG_StringConst(char s[], int id) {
   gen(".STR%d:\n\t.string\t\"%s\"\n", id, s);
}

void load(enum regs R, opts x) {
   // TODO: SIGN EXTEND!
   SymbolEntry *s;
   enum dataTypes t, t1, t2, t3, t4;
   char *r[10];
   char *m;

   switch (x.type) {
      case CONST: // arithmetic constant
         t = trans(typeInteger);
         gen("\tmov%c\t$%d, %s\n", cmd(t), x.content.constant, reg(R, t)); 
         break;
      case VAR:  // Symbol table entry (variable, constant, function, parameter, temporary)
         s = x.content.variable;
         switch (s->entryType) {
            case ENTRY_VARIABLE:
               t = trans(s->u.eVariable.type);

               if (s->nestingLevel == 2) // Global Variable
                  gen("\tmov%c\t%s(%%rip), %s\n", cmd(t), s->id, reg(R, t));
               else // Local Variable
                  gen("\tmov%c\t%d(%%rbp), %s\n", cmd(t), s->u.eVariable.offset, reg(R, t));

               break;
            case ENTRY_PARAMETER:
               // TODO: by val vs by ref
               t = trans(s->u.eParameter.type);
               //if (s->u.eParameter.offset < 6) // in register
               //   gen("\tmov%c\t%s, %s\n", cmd(t), reg(args[s->u.eParameter.offset], t), reg(R, t));
               //else // in stack
               gen("\tmov%c\t%d(%%rbp), %s\n", cmd(t), s->u.eParameter.offset, reg(R, t));
               break;
            case ENTRY_CONSTANT:
               t = trans(s->u.eConstant.type);
               switch (t) {
                  case INT: case BOOL:
                     gen("\tmov%c\t$%d, %s\n", cmd(t), s->u.eConstant.value.vInteger, reg(R, t));
                     break;
                  case CHAR:
                     gen("\tmovb\t$0x%x, %s\n", s->u.eConstant.value.vChar, reg(R, t));
                     break;
                  case POINTER:
                     gen("\tmovq\t$.STR%d, %s\n", s->u.eConstant.id, reg(R, t));
                     break;
                  case REAL:
                     /* TODO */
                     break;
               }
               break;
            case ENTRY_TEMPORARY:
               t = trans(s->u.eTemporary.type);
               gen("\tmov%c\t%d(%%rbp), %s # tmp%d\n", cmd(t), s->u.eTemporary.offset, reg(R, t), s->u.eTemporary.number);
               break;
            default:
               break;
         }
         
      default:
         break;
   }

}

void store(enum regs R, opts x) {
   // TODO: SIGN EXTEND!
   SymbolEntry *s;
   enum dataTypes t;

   switch (x.type) {
      case CONST: // arithmetic constant
         internal("\rtarget.c:[store]: cannot store on a constant");
         break;
      case VAR:  // Symbol table entry (variable, constant, function, parameter, temporary)
         s = x.content.variable;
         switch (s->entryType) {
            case ENTRY_CONSTANT:
               internal("\rtarget.c:[store]: cannot store on a constant");
               break;
            case ENTRY_VARIABLE:
               t = trans(s->u.eVariable.type);

               if (s->nestingLevel == 2) // Global Variable
                  gen("\tmov%c\t%s, %s(%%rip)\n", cmd(t), reg(R, t), s->id);
               else // Local Variable
                  gen("\tmov%c\t%s, %d(%%rbp)\n", cmd(t), reg(R, t), s->u.eVariable.offset);
               break;
            case ENTRY_TEMPORARY:
               t = trans(s->u.eTemporary.type);
               gen("\tmov%c\t%s, %d(%%rbp)\n", cmd(t), reg(R, t), s->u.eTemporary.offset);
               break;
            case ENTRY_PARAMETER:
               // TODO: by reference arguments
               t = trans(s->u.eTemporary.type);
               //if (s->u.eParameter.offset < 6) // in register
               //   gen("\tmov%c\t%s, %s\n", cmd(t), reg(R, t), reg(args[s->u.eParameter.offset], t));
               //else // in stack
               gen("\tmov%c\t%s, %d(%%rbp)\n", cmd(t), reg(R, t), s->u.eParameter.offset);
               break;
            default:
               break;
         }
         break;
      default:
         break;
   }
}

void TG_quad(immType q) {
   SymbolEntry *s;
   enum dataTypes t;
   int cnt = 0, i, size;
   static int parcnt = 0, argStackSize = 0;
   static opts ret;

   switch (q.op) {
      case UNIT:
         currentFunc = q.x.content.variable;
         gen("\t.globl\t%s\n", currentFunc->id);
         gen("\t.type\t%s, @function\n", currentFunc->id);
         gen("%s:\n", currentFunc->id);

         /* Count how many bytes of stack to allocate */
         cnt = 8; // Add 8 bytes for the bp pointer
         for (s = q.x.scope->entries; s != NULL; s = s->nextInScope)
            if (s->entryType == ENTRY_VARIABLE || s->entryType == ENTRY_TEMPORARY)
               cnt += sizeOfType(getSymType(s));

         gen("\tpushq\t%%rbp\n");
         gen("\tmovq\t%%rsp, %%rbp\n");
         gen("\tsubq\t$%d,%%rsp\n", cnt);

         /* Save arguments into the stack  */
         /*
         for (i = 0, s = currentFunc->u.eFunction.firstArgument;
              s != NULL;
              i++, s = s->u.eParameter.next)
            if (i < 6)
               store(args[i], Var(s));
         */
         
         /*
         if (currentFunc->u.eFunction.firstArgument)
            argStackSize = currentFunc->u.eFunction.firstArgument->u.eParameter.offset
                         + sizeOfType(currentFunc->u.eFunction.firstArgument->u.eParameter.type);
         else
            argStackSize = 0;
         */
         break;
      case ENDU:
         if (!strcmp(q.x.content.variable->id, "main")) {
           gen("\tmovl\t$0, %%edi\n");
           gen("\tcall\texit\n");
         } else {
            gen(".%s:\n", q.x.content.variable->id);
            gen("\tmovq\t%%rbp, %%rsp\n");
            gen("\tpopq\t%%rbp\n");
            //gen("\tsubl\t$%d, %%esp\n", argStackSize);
            gen("\tret\n");
         }
         break;
      case ASG:
         load(AX, q.x);
         store(AX, q.z);
         break;
      case '+': case '-': case '*':
         load(AX, q.x);
         load(BX, q.y);

         s = q.z.content.variable;
         t = trans(getSymType(s));

         gen("\t%s%c\t%s, %s\n", arithmCmd(q.op), cmd(t), reg(BX, t), reg(AX, t));

         store(AX, q.z);
         break;
      case '/': case '%':
         gen("\tpushq\t%%rdx\n"); // Save %rdx

         load(AX, q.x);
         load(BX, q.y);

         gen("\tcqto\n");        // Sign extend %rax to %rdx:%rax
         gen("\tidivq\t%%rbx\n");

         if (q.op == '/') store(AX, q.z);
         else             store(DX, q.z);

         gen("\tpopq\t%%rdx\n");  // Restore %rdx
         break;
      case '=': case '<': case ',': case '>': case '.': case '!':
         load(AX, q.x);
         load(BX, q.y);
         gen("\tcmpq\t%%rbx, %%rax\n");
         gen("\tj%s\t.L%d\n", compCmd(q.op), q.z.content.label);
         break;
      case IFB:
         load(AX, q.x);
         gen("\ttestb\t%s, %s\n", reg(AX, BOOL), reg(AX, BOOL));
         gen("\tjnz\t.L%d\n", q.z.content.label);
         break;
      case JUMP:
         gen("\tjmp\t.L%d\n", q.z.content.label);
         break;
      case PAR:
         switch (q.y.content.mode) {
            case PASS_BY_VALUE:
               t = trans(getVarType(q.x));

               size = sizeOfType(getVarType(q.x));
               argStackSize += size;

               gen("\tsubq\t$%d, %%rsp\n", size);

               if (parcnt < 6)
                  load(args[parcnt], q.x);

               load(AX, q.x);
               gen("\tmov%c\t%s, (%%rsp)\n", cmd(t), reg(AX, t));
               break;
            case PASS_BY_REFERENCE:
               break;
            case PASS_RET:
               ret = q.x;
               break;
         }
         parcnt++;
         break;
      case CALL:
         gen("\tcall\t%s\n", q.z.content.variable->id);
         if (!equalType(q.z.content.variable->u.eFunction.resultType, typeVoid))
            store(AX, ret);
         gen("\taddq\t$%d, %%rsp\n", argStackSize);
         argStackSize = 0;
         parcnt = 0;
         break;
      case RET:
         gen("\tjmp\t.%s\n", currentFunc->id);
         break;
      case RETV:
         load(AX, q.x);
         gen("\tjmp\t.%s\n", currentFunc->id);
         break;
      default:
         break;
   }
}

void initGlobal(immType q) {
   SymbolEntry *s;

   if (q.op != ASG)
      internal("[target.c]:TG_Generate: Found non assignment command in global scope");
   if (q.z.type != VAR)
      internal("[target.c]:TG_Generate: Assignment to non-variable in global scope");
   if (q.x.type != VAR && q.x.content.variable->entryType != ENTRY_CONSTANT)
      internal("[target.c]:TG_Generate: rvalue in global initialization is not a constant");

   s = q.z.content.variable;

   gen("\t.globl\t%s\n", s->id);
   gen("\t.data\n");
   //gen("\t.align\t4\n");
   gen("\t.type\t%s, @object\n", s->id);
   gen("\t.size\t%s, %d\n", s->id, sizeOfType(getSymType(s)));
   gen("%s:\n", s->id);

   s = q.x.content.variable;
   switch (s->u.eConstant.type->kind) {
      case TYPE_INTEGER:
         gen("\t.long\t%d\n", s->u.eConstant.value.vInteger);
         break;
      case TYPE_CHAR:
         gen("\t.byte\t%d\n", s->u.eConstant.value.vChar);
         break;
      case TYPE_BOOLEAN:
         gen("\t.byte\t%d\n", s->u.eConstant.value.vBoolean);
         break;
      case TYPE_REAL:
         break;
      default:
         internal("[target.c]:initGlobal: invalid type in global variable initialization");
   }
}

void TG_Generate() {
   int i;

   // Global variables: allocation and initialization
   for (i = 1; i <= immCurrentPos; i++) {
      if (immCode[i].op == UNIT) break;
      initGlobal(immCode[i]);
   }

   gen("\t.text\n");

   // Code for functions
   for (; i <= immCurrentPos; i++) {
      label(i);
      TG_quad(immCode[i]); 
   }

}
