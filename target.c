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

#define REGS 13
enum regs { AX, BX, CX, DX, SI, DI, BP, SP, R8, R9, R10, R13, R14 };
char regName[DATATYPES][REGS][6] =
{
   { "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp", "%r8d", "%r9d", "%r10d", "%r13d", "%r14d"},
   {  "%al",  "%bl",  "%cl",  "%dl", "%sil", "%dil", "%bpl", "%spl", "%r8b", "%r9b", "%r10b", "%r13b", "%r14b"},
   {  "%al",  "%bl",  "%cl",  "%dl", "%sil", "%dil", "%bpl", "%spl", "%r8b", "%r9b", "%r10b", "%r13b", "%r14b"},
   { "%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%rbp", "%rsp",  "%r8",  "%r9",  "%r10",  "%r13",  "%r14"}
};
enum regs args[6] = { DI, SI, DX, CX, R8, R9 };
//int revArgs[REGS] = {-1, -1, 3, 2, 1, 0, -1, -1, 4, 5 };
//SymbolEntry *argsSymEntry[6];  /* Symbol Table entries for the first 6 arguments */

SymbolEntry *currentFunc = NULL;;

void label(int i) {
   gen(".L%d:\n", i);
}

/* Translates a high level type to a low level dataTypes */
enum dataTypes trans(Type t) {
   if (equalType(t, typeInteger))
      return INT;
   else if (equalType(t, typeChar))
      return CHAR;
   else if (equalType(t, typeBoolean))
      return BOOL;
   else if (t->kind == TYPE_ARRAY || t->kind == TYPE_IARRAY || t->kind == TYPE_POINTER)
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
      case CONST:   return typeInteger;
      case VAR:     return getSymType(x.content.variable);
      case REF_VAR: return x.content.variable->u.eTemporary.type->refType;
      default:
         internal("\r[target.c]:getVarType: invalid opts arguments");
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
   // TODO: REALs
   SymbolEntry *s;
   enum dataTypes t;

   switch (x.type) {
      case CONST: // arithmetic constant
         t = INT;
         gen("\tmov%c\t$%d, %s\n", cmd(t), x.content.constant, reg(R, t)); 
         break;
      case VAR:  // Symbol table entry (variable, constant, function, parameter, temporary)
         s = x.content.variable;
         switch (s->entryType) {
            case ENTRY_VARIABLE:
               t = trans(s->u.eVariable.type);

               if (s->nestingLevel == 2) // Global Variable
                  gen("\tmov%c\t%s(%%rip), %s\n", cmd(t), s->id, reg(R, t));
               else                      // Local Variable
                  gen("\tmov%c\t%d(%%rbp), %s\n", cmd(t), s->u.eVariable.offset, reg(R, t));

               break;
            case ENTRY_PARAMETER:
               t = trans(s->u.eParameter.type);
               if (s->u.eParameter.mode == PASS_BY_VALUE) // Pass by value
                  gen("\tmov%c\t%d(%%rbp), %s\n", cmd(t), s->u.eParameter.offset, reg(R, t));
               else {                                     // Pass by reference
                  gen("\tmov%c\t%d(%%rbp), %%r10\n", cmd(POINTER), s->u.eParameter.offset);
                  gen("\tmov%c\t(%%r10), %s\n", cmd(t), reg(R, t));
               }
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
         break;
      case REF_VAR:
         s = x.content.variable;

         if (s->entryType != ENTRY_TEMPORARY)
            internal("\r[target.c]:load: x in [x] is not a temporary variable");

         t = trans(s->u.eTemporary.type->refType);

         load(R10, Var(s));
         gen("\tmov%c\t(%%r10), %s\n", cmd(t), reg(R, t));
         break;
      default:
         break;
   }
}

void loadAddr(enum regs R, opts x) {
   SymbolEntry *s;

   switch (x.type) {
      case CONST:
         internal("\r[target.c]:loadAddr: Cannot load the address of a constant (1)");
         break;
      case VAR:
         s = x.content.variable;
         switch (s->entryType) {
            case ENTRY_CONSTANT:
               internal("\r[target.c]:loadAddr: Cannot load the address of a constant (2)");
               break;
            case ENTRY_VARIABLE:
               if (s->nestingLevel == 2) // Global Variable
                  gen("\tleaq\t$%s(%%rip), %s\n", s->id, reg(R, POINTER));
               else                      // Local Variable
                  gen("\tleaq\t%d(%%rbp), %s\n", s->u.eVariable.offset, reg(R, POINTER));
               break;
            case ENTRY_TEMPORARY:
               gen("\tleaq\t%d(%%rbp), %s\n", s->u.eTemporary.offset, reg(R, POINTER));
               break;
            case ENTRY_PARAMETER:
               if (s->u.eParameter.mode == PASS_BY_VALUE) // Pass by value
                  gen("\tleaq\t%d(%%rbp), %s\n", s->u.eParameter.offset, reg(R, POINTER));
               else                                       // Pass by reference
                  gen("\tmovq\t%d(%%rbp), %s\n", s->u.eParameter.offset, reg(R, POINTER));
               break;
            default:
               internal("\r[target.c]:loadAddr: Invalid entry type");
               break;
         }
         break;
      case REF_VAR:
         s = x.content.variable;
         load(R, Var(s));
         break;
      default:
         internal("\r[target.c]:loadAddr: Cannot load the address of this object");
         break;
   }
}

void store(enum regs R, opts x) {
   // TODO: SIGN EXTEND!
   // TODO: REALs
   SymbolEntry *s;
   enum dataTypes t;

   switch (x.type) {
      case CONST: // arithmetic constant
         internal("\rtarget.c:[store]: cannot store on a constant (1)");
         break;
      case VAR:  // Symbol table entry (variable, constant, function, parameter, temporary)
         s = x.content.variable;
         switch (s->entryType) {
            case ENTRY_CONSTANT:
               internal("\rtarget.c:[store]: cannot store on a constant (2)");
               break;
            case ENTRY_VARIABLE:
               t = trans(s->u.eVariable.type);

               if (s->nestingLevel == 2) // Global Variable
                  gen("\tmov%c\t%s, %s(%%rip)\n", cmd(t), reg(R, t), s->id);
               else                      // Local Variable
                  gen("\tmov%c\t%s, %d(%%rbp)\n", cmd(t), reg(R, t), s->u.eVariable.offset);
               break;
            case ENTRY_TEMPORARY:
               t = trans(s->u.eTemporary.type);
               gen("\tmov%c\t%s, %d(%%rbp)\n", cmd(t), reg(R, t), s->u.eTemporary.offset);
               break;
            case ENTRY_PARAMETER:
               t = trans(s->u.eParameter.type);
               if (s->u.eParameter.mode == PASS_BY_VALUE) // Pass by value
                  gen("\tmov%c\t%s, %d(%%rbp)\n", cmd(t), reg(R, t), s->u.eParameter.offset);
               else {                                     // Pass by reference
                  gen("\tmov%c\t%d(%%rbp), %%r10\n", cmd(POINTER), s->u.eParameter.offset);
                  gen("\tmov%c\t%s, (%%r10)\n", cmd(t), reg(R, t));
               }
               break;
            default:
               break;
         }
         break;
      case REF_VAR:
         s = x.content.variable;

         if (s->entryType != ENTRY_TEMPORARY)
            internal("\r[target.c]:store: x in [x] is not a temporary variable");

         t = trans(s->u.eTemporary.type->refType);

         load(R10, Var(s));
         gen("\tmov%c\t%s, (%%r10)\n", cmd(t), reg(R, t));
         break;
      default:
         break;
   }
}

void TG_quad(immType q) {
   SymbolEntry *s;
   Type rt;
   enum dataTypes t;
   int cnt = 0, size;
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
            if (s->entryType == ENTRY_VARIABLE || s->entryType == ENTRY_TEMPORARY) {
               rt = getSymType(s);
               cnt += sizeOfType(rt, true);

              /*
               * For local arrays we must also allocate
               * a pointer to the location of the first element
               */
               if (rt->kind == TYPE_ARRAY)
                  cnt++;
            }


         gen("\tpushq\t%%rbp\n");
         gen("\tmovq\t%%rsp, %%rbp\n");
         gen("\tsubq\t$%d,%%rsp\n", cnt);

         for (s = q.x.scope->entries; s != NULL; s = s->nextInScope)
            if (s->entryType == ENTRY_VARIABLE && getSymType(s)->kind == TYPE_ARRAY) {
               gen("\tleaq\t%d(%%rbp), %%rax\n", s->u.eVariable.offset+8);
               gen("\tmovq\t%%rax, %d(%%rbp)\n", s->u.eVariable.offset);
            }

         break;
      case ENDU:
         if (!strcmp(q.x.content.variable->id, "main")) {
            gen("\tmovl\t$0, %%edi\n");
            gen("\tcall\texit\n");
         } else {
            gen(".%s:\n", q.x.content.variable->id);
            gen("\tmovq\t%%rbp, %%rsp\n");
            gen("\tpopq\t%%rbp\n");
            gen("\tret\n");
         }
         break;
      case ASG:
         load(AX, q.x);
         store(AX, q.z);
         break;
      case ARRAY:
         load(AX, q.y);
         s = q.x.content.variable;

         // Find the type of objects held in the array
         rt = getSymType(s);
         while (rt->kind == TYPE_ARRAY || rt->kind == TYPE_IARRAY)
            rt = rt->refType;

         gen("\tmovl\t$%d, %s\n", sizeOfType(rt, false), reg(R10, INT));
         gen("\timul%c\t%s, %s\n", cmd(POINTER), reg(R10, POINTER), reg(AX, POINTER));

         load(R10, q.x);
         gen("\tadd%c\t%s, %s\n", cmd(POINTER), reg(R10, POINTER), reg(AX, POINTER));

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
         /* WRONG!!! */
         /*
         switch(q.y.content.mode) {
            case PASS_BY_VALUE: case PASS_BY_REFERENCE:
               if (q.x.type == CONST) { // Integer constant
                  if (q.y.content.mode == PASS_BY_VALUE) {
                     t = INT;
                     size = 4;
                     f = load;
                  } else
                     internal("\r[target.c]:TG_Quad: cannot pass integer constant by reference (1)");
               } else if (q.x.type == VAR) {
                  if (q.x.content.variable->entryType == ENTRY_CONSTANT) { // Int/Char/Bool/String Constant
                     if (q.y.content.mode == PASS_BY_VALUE) {
                        t = trans(getVarType(q.x));
                        size = sizeOfType(getVarType(q.x), false);
                        f = load;
                     } else
                        internal("\r[target.c]:TG_Quad: cannot pass integer constant by reference (2)");
                  } else { // Varible, Temporary, Parameter
                     rt = getSymType(q.x.content.variable);
                     if (rt->kind == TYPE_ARRAY || rt->kind == TYPE_IARRAY) {
                        t = POINTER;
                        size = 8;
                        f = loadAddr;
                     } else if (q.y.content.mode == PASS_BY_VALUE) {
                        t = trans(getVarType(q.x));
                        size = sizeOfType(getVarType(q.x), false);
                        f = load;
                     } else {
                        t = POINTER;
                        size = 8;
                        f = loadAddr;
                     }
                  }
               } else if (q.x.type == REF_VAR) {
                  if (q.y.content.mode == PASS_BY_VALUE) {
                     t = trans(getVarType(q.x));
                     size = sizeOfType(getVarType(q.x), false);
                     f = load;
                  } else {
                     t = POINTER;
                     size = 8;
                     f = loadAddr;
                  }
               }

               argStackSize += size;
               gen("\tsubq\t$%d, %%rsp\n", size);

               if (parcnt < 6)
                  f(args[parcnt], q.x);

               f(AX, q.x);
               gen("\tmov%c\t%s, (%%rsp)\n", cmd(t), reg(AX, t));
               break;
            */
         switch (q.y.content.mode) {
            case PASS_BY_VALUE:
               t = trans(getVarType(q.x));

               size = sizeOfType(getVarType(q.x), false);
               argStackSize += size;

               gen("\tsubq\t$%d, %%rsp\n", size);

               if (parcnt < 6)
                  load(args[parcnt], q.x);

               load(AX, q.x);
               gen("\tmov%c\t%s, (%%rsp)\n", cmd(t), reg(AX, t));
               break;
            case PASS_BY_REFERENCE:
               size = 8; // size of pointer
               argStackSize += size;

               gen("\tsubq\t$%d, %%rsp\n", size);

               if (parcnt < 6)
                  loadAddr(args[parcnt], q.x);

               loadAddr(AX, q.x);
               gen("\tmov%c\t%s, (%%rsp)\n", cmd(POINTER), reg(AX, POINTER));
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
      internal("\r[target.c]:TG_Generate: Found non assignment command in global scope");
   if (q.z.type != VAR)
      internal("\r[target.c]:TG_Generate: Assignment to non-variable in global scope");
   if (q.x.type != VAR && q.x.content.variable->entryType != ENTRY_CONSTANT)
      internal("\r[target.c]:TG_Generate: rvalue in global initialization is not a constant");

   s = q.z.content.variable;

   gen("\t.globl\t%s\n", s->id);
   gen("\t.data\n");
   //gen("\t.align\t4\n");
   gen("\t.type\t%s, @object\n", s->id);
   gen("\t.size\t%s, %d\n", s->id, sizeOfType(getSymType(s), true));
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
         internal("\r[target.c]:initGlobal: invalid type in global variable initialization");
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
