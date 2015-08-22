#include "target.h"
#include "intermediateCode.h"
#include "symbol.h"
#include "general.h"
#include "error.h"

int StringID = 0;
int newStringID() { return StringID++; }

enum regs { AX, BX, CX, DX, SI, BP, SP };
char regName[][10] = { "ax", "bx", "cx", "dx", "si", "bp", "sp" };

char *reg(enum regs R, char *size) {
   static char res[10];
   sprintf(res, "%%%s%s", size, regName[R]);
   return res;
}

void TG_Preamble() {
   fprintf(asmfile, "\t.file \"%s\"\n", filename);
   fprintf(asmfile, "\t.section .rodata\n");
}

void TG_StringConst(char s[], int id) {
   fprintf(asmfile, ".STR%d:\n\t.string\t\"%s\"\n", id, s);
}

// TODO: the result can only be used before the next call
char *modifier(Type t) {
   if (equalType(t, typeInteger))
      return "e";
   else if (equalType(t, typeChar) || equalType(t, typeBoolean))
      return "";
   else
      return "q";
}

void load(enum regs R, opts x) {
   SymbolEntry *s;
   char *m;

   switch (x.type) {
      case CONST: // arithmetic constant
         fprintf(asmfile, "\tmovq\t$%d, %s\n", x.content.constant, reg(R, "e")); 
         break;
      case VAR:  // Symbol table entry (variable, constant, function, parameter, temporary)
         s = x.content.variable;
         switch (s->entryType) {
            case ENTRY_VARIABLE:
               m = modifier(s->u.eVariable.type);
               fprintf(asmfile, "\tmov%s\t%d(%%rbp), %s\n", m, s->u.eVariable.offset, reg(R, m));
               //asm("int $3"); // Breakpoint
               break;
            case ENTRY_PARAMETER:
               m = modifier(s->u.eParameter.type);
               fprintf(asmfile, "\tmov%s\t%d(%%rbp), %s\n", m, s->u.eParameter.offset, reg(R, m));
               //asm("int $3"); // Breakpoint
               break;
            default:
               break;
         }
         
      default:
         break;
   }

}

void store(enum regs R, opts x) {
   /* TODO */
}

void TG_quad(immType q) {
   switch (q.op) {
      case UNIT:
         fprintf(asmfile, "%s:\n", q.x.content.variable->id);
         fprintf(asmfile, "\tpushq\t%%rbp\n");
         break;
      case ASG:
         load(AX, q.x);
         store(AX, q.z);
         break;
      case '+':
         load(AX, q.x);
         load(DX, q.y);
         fprintf(asmfile, "\tadd%s\t%s, %s\n", "e", reg(DX, "e"), reg(AX, "e"));
         store(AX, q.z);
      default:
         break;
   }
}

void TG_Generate() {
   int i;

   fprintf(asmfile, "\t.text\n");

   for (i = 1; i <= immCurrentPos; i++) {
      TG_quad(immCode[i]); 
   }

}
