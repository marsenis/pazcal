#include <stdio.h>
#include <stdlib.h>

#include "general.h"
#include "error.h"
#include "symbol.h"
#include "intermediateCode.h"

immType immCode[MAX];
int immCurrentPos = 0;

int nextQuad() { return immCurrentPos + 1; }

void genQuad(enum opType op, opts x, opts y, opts z) {
   immCurrentPos++;
   immCode[ immCurrentPos ] = (immType) { op, x, y, z };
}

labelListType *emptyList() { return NULL; }
labelListType *makeList(int x) {
   labelListType *p;
   p = (labelListType *)new(sizeof(labelListType));
   p->label = x;
   p->nxt = NULL;
   return p;
}
labelListType *mergeLists(labelListType *l1, labelListType *l2) {
   labelListType *p;
   for (p = l1; p != NULL && p->nxt != NULL; p = p->nxt) ;

   if (p == NULL) return l2;
   p->nxt = l2;

   return l1;
}

// TODO: maybe destroy the list after backpatching.
//       Consider using garbage collector because most
//       of the label lists are not freed
void backpatch(labelListType *l, int z) {
   labelListType *p;
   for (p = l; p != NULL; p = p->nxt)  {
      immCode[p->label].z.content.label = z;
      immCode[p->label].z.type = LBL;
   }
}

/* TODO: finish printing function (standard printing) */

/* TODO: as it is now, fix sometimes allocates space for the string it returns
 *       and sometimes the string is already allocated (if it's a
 *       variable/function name). So there is no way to tell later if we
 *       need to free the corresponding memory. Also using static variable
 *       for tmp is not going to work because fix is called multiple
 *       times inside printf and C doesn't define the order of evalutation.
 */
const char *fix(opts id) {
   char *tmp;

   switch (id.type) {
      case CONST:
         tmp = calloc(64, sizeof(char));
         sprintf(tmp, "%d", id.content.constant);
         return tmp;
         break;
      case VAR:
         tmp = calloc(64, sizeof(char));
         switch (id.content.variable->entryType) {
            case ENTRY_CONSTANT:
               switch (id.content.variable->u.eConstant.type->kind) {
                  case TYPE_INTEGER:
                     sprintf(tmp, "%d", id.content.variable->u.eConstant.value.vInteger);
                     break;
                  case TYPE_BOOLEAN:
                     sprintf(tmp, "%s", (id.content.variable->u.eConstant.value.vBoolean) ? "true" : "false" );
                     break;
                  case TYPE_CHAR:
                     sprintf(tmp, "%c", id.content.variable->u.eConstant.value.vChar);
                     break;
                  case TYPE_REAL:
                     sprintf(tmp, "%lf", id.content.variable->u.eConstant.value.vReal);
                     break;
                  case TYPE_ARRAY: case TYPE_IARRAY:
                     if (id.content.variable->u.eConstant.type->refType->kind == TYPE_CHAR)
                        sprintf(tmp, "%s", id.content.variable->u.eConstant.value.vString);
                     else
                        internal("found a constant with non-regular type");
                     break;
                  default:
                     internal("found a constant with non-regular type");
                     break;
               }
               return tmp;
               break;
            case ENTRY_TEMPORARY:
               tmp = calloc(64, sizeof(char));
               if ( id.content.variable->u.eTemporary.type->kind == TYPE_POINTER )
                  sprintf(tmp, "[%s]", id.content.variable->id);
               else
                  sprintf(tmp, "$%d", id.content.variable->u.eTemporary.number);
               return tmp;
               break;
            default:
               return id.content.variable->id;
               break;
         }
         break;
      case LBL:
         tmp = calloc(64, sizeof(char));
         sprintf(tmp, "%d", id.content.label);
         return tmp;
         break;
      case PASS:
         switch (id.content.mode) {
            case PASS_BY_VALUE: return "V"; //return "by value";
            case PASS_BY_REFERENCE: return "R"; //return "by reference";
            case PASS_RET: return "RET"; //return "by return value";
         }
         break;
      case EMPTY:
         return "*";
         break;
   }
}

void printImm() {
   int i;
   for (i=1; i<=immCurrentPos; i++) {
      switch (immCode[i].op) {
         case UNIT:
            printf("%d: unit %s,-,-\n", i, fix(immCode[i].x));
            break;
         case ENDU:
            printf("%d: endu %s,-,-\n", i, fix(immCode[i].x));
            break;
         case ARRAY:
            printf("%d: array %s, %s, %s\n", i, fix(immCode[i].x), fix(immCode[i].y), immCode[i].z.content.variable->id);
            break;
         case '=': case '>': case '<':
            printf("%d: %c, %s, %s, %s\n", i, immCode[i].op, fix(immCode[i].x), fix(immCode[i].y), fix(immCode[i].z) );
            break;
         case '+': case '-': case '*': case '/': case '%':
            printf("%d: %s := %s %c %s\n", i, fix(immCode[i].z), fix(immCode[i].x), immCode[i].op, fix(immCode[i].y) );
            break;
         case ASG:
            printf("%d: %s := %s\n", i, fix(immCode[i].z), fix(immCode[i].x));
            break;
         case '!':
            printf("%d: <>, %s, %s, %s\n", i, fix(immCode[i].x), fix(immCode[i].y), fix(immCode[i].z) );
            break;
         case ',':
            printf("%d: <=, %s, %s, %s\n", i, fix(immCode[i].x), fix(immCode[i].y), fix(immCode[i].z) );
            break;
         case '.':
            printf("%d: >=, %s, %s, %s\n", i, fix(immCode[i].x), fix(immCode[i].y), fix(immCode[i].z) );
            break;
         case IFB:
            printf("%d: if %s then jump %s\n", i, fix(immCode[i].x), fix(immCode[i].z));
            break;
         case JUMP:
            printf("%d: jump, -, -, %s\n", i, fix(immCode[i].z));
            break;
         case LABEL:
            printf("%d: Label %s\n", i, fix(immCode[i].x));
            break;
         case JUMPL:
            printf("%d: Jump to label %s\n", i, fix(immCode[i].z));
            break;
         case CALL:
            printf("%d: call -, -, %s\n", i, fix(immCode[i].z));
            //printf("%d: Call unit %s\n", i, fix(immCode[i].z));
            break;
         case PAR:
            printf("%d: pass %s, %s, -\n", i, fix(immCode[i].x), fix(immCode[i].y));
            //printf("%d: Pass parameter %s with mode %s\n", i, fix(immCode[i].x), fix(immCode[i].y));
            break;
         case RET:
            printf("%d: ret, -, -, -\n", i);
            break;
         case RETV:
            printf("%d: retv, %s, -, -\n", i, fix(immCode[i].x));
            break;
         default:
            printf("[Error] Unknown imm command\n");
            break; 
      }
   }
}
