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
enum regs results[6] = { DI, SI, DX, CX, R8, R9 };

void label(int i) {
   gen("L%d:\n", i);
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

void TG_Preamble() {
   gen("\t.file \"%s\"\n", filename);
   gen("\t.section .rodata\n");
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
               // TODO: this does not work with global variables
               t = trans(s->u.eVariable.type);
               gen("\tmov%c\t%d(%%rbp), %s\n", cmd(t), s->u.eVariable.offset, reg(R, t));
               //asm("int $3"); // Breakpoint
               break;
            case ENTRY_PARAMETER:
               // TODO: by val vs by ref
               // TODO: first 6 parameters are on the registers
               t = trans(s->u.eParameter.type);
               gen("\tmov%c\t%d(%%rbp), %s\n", cmd(t), s->u.eParameter.offset, reg(R, t));
               //asm("int $3"); // Breakpoint
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
               // TODO: this does not work for global variables
               t = trans(s->u.eVariable.type);
               gen("\tmov%c\t%s, %d(%%rbp)\n", cmd(t), reg(R, t), s->u.eVariable.offset);
               break;
            case ENTRY_TEMPORARY:
               t = trans(s->u.eTemporary.type);
               gen("\tmov%c\t%s, %d(%%rbp)\n", cmd(t), reg(R, t), s->u.eTemporary.offset);
               break;
            default:
               break;
         }
      default:
         break;
   }
}

char *arithmCmd(enum opType op) {
   switch (op) {
      case '+': return "add";
      case '-': return "sub";
      case '*': return "imul";
      default: return "";
   }
}

void TG_quad(immType q) {
   SymbolEntry *s;
   enum dataTypes t;
   int cnt = 0;
   static int parcnt = 0;
   static opts ret;

   switch (q.op) {
      case UNIT:
         gen("\t.globl\t%s\n", q.x.content.variable->id);
         gen("\t.type\t%s, @function\n", q.x.content.variable->id);
         gen("%s:\n", q.x.content.variable->id);
         for (s = q.x.scope->entries; s != NULL; s = s->nextInScope) {
            if (s->entryType != ENTRY_VARIABLE && s->entryType != ENTRY_TEMPORARY)
               continue;
            cnt += sizeOfType(s->u.eVariable.type); // TODO: a little hackish
         }
         cnt += 8; // Add 8 bytes for the bp pointer
         gen("\tpushq\t%%rbp\n");
         gen("\tmovq\t%%rsp, %%rbp\n");
         gen("\tsubq\t$%d,%%rsp\n", cnt);
         break;
      case ENDU:
         gen(".%s:\n", q.x.content.variable->id);
         gen("\tmovq\t%%rbp, %%rsp\n");
         gen("\tpopq\t%%rbp\n");
         gen("\tret\n");
         break;
      case ASG:
         load(AX, q.x);
         store(AX, q.z);
         break;
      case '+': case '-': case '*': case '/': case '%':
         load(AX, q.x);
         load(DX, q.y);

         s = q.z.content.variable;
         t = trans(s->u.eVariable.type); // TODO: a little hackish

         gen("\t%s%c\t%s, %s\n", arithmCmd(q.op), cmd(t), reg(DX, t), reg(AX, t));

         store(AX, q.z);
         break;
      case PAR:
         switch (q.y.content.mode) {
            case PASS_BY_VALUE:
               load(results[parcnt], q.x); // TODO: This only works for 1-6th argument
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
         parcnt = 0;
         break;
      default:
         break;
   }
}

void TG_Generate() {
   int i;

   gen("\t.text\n");

   for (i = 1; i <= immCurrentPos; i++) {
      label(i);
      TG_quad(immCode[i]); 
   }

}
