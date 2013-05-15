#include <stdlib.h>
#include <stdio.h>

#include "symbol.h"


/* Äéáäéêáóßá åêôýðùóçò ôïõ ðßíáêá óõìâüëùí */

//#define SHOW_OFFSETS

void printSymbolTable ()
{
    Scope       * scp;
    SymbolEntry * e;
    SymbolEntry * args;
    
    scp = currentScope;
    if (scp == NULL)
        printf("no scope\n");
    else
        while (scp != NULL) {
            printf("scope: ");
            e = scp->entries;
            while (e != NULL) {
                if (e->entryType == ENTRY_TEMPORARY)
                    printf("$%d", e->u.eTemporary.number);
                else if (e->entryType == ENTRY_CONSTANT) {
                    if ( equalType( e->u.eConstant.type, typeInteger ) )
                       printf("%s[%d]", e->id, e->u.eConstant.value.vInteger);
                    else if ( equalType( e->u.eConstant.type, typeBoolean ) )
                       printf("%s[%s]", e->id, e->u.eConstant.value.vBoolean ? "true" : "false");
                    else if ( equalType( e->u.eConstant.type, typeReal ) )
                       printf("%s[%lf]", e->id, e->u.eConstant.value.vReal);
                    else if ( equalType( e->u.eConstant.type, typeChar ) )
                       printf("%s[%c]", e->id, e->u.eConstant.value.vChar);
                } else 
                    printf("%s", e->id);
                switch (e->entryType) {
                    case ENTRY_FUNCTION:
                        printf("(");
                        args = e->u.eFunction.firstArgument;
                        while (args != NULL) {
                            printMode(args->u.eParameter.mode);
                            printf("%s : ", args->id);
                            printType(args->u.eParameter.type);
                            args = args->u.eParameter.next;
                            if (args != NULL)
                                printf("; ");
                        }
                        printf(") : ");
                        printType(e->u.eFunction.resultType);
                        break;
                    case ENTRY_VARIABLE:
                        printf("[");
                        printType(e->u.eVariable.type);
                        printf("]");
                        break;
                    case ENTRY_PARAMETER:
                        printf("[");
                        printType(e->u.eParameter.type);
                        printf("]");
                        break;
                    case ENTRY_TEMPORARY:
                        printf("[");
                        printType(e->u.eTemporary.type);
                        printf("]");
                        break;
#ifdef SHOW_OFFSETS
                    case ENTRY_VARIABLE:
                        printf("[%d]", e->u.eVariable.offset);
                        break;
                    case ENTRY_PARAMETER:
                        printf("[%d]", e->u.eParameter.offset);
                        break;
                    case ENTRY_TEMPORARY:
                        printf("[%d]", e->u.eTemporary.offset);
                        break;
#endif
                }
                e = e->nextInScope;
                if (e != NULL)
                    printf(", ");
            }
            scp = scp->parent;
            printf("\n");
        }
    printf("----------------------------------------\n");
}

void printMismatch(Type t1, Type t2) {
#ifdef DEBUG_SYMBOL
   printf("Type missmatch ");
   printType(t1);
   printf(", ");
   printType(t2);
   printf("\n");
#endif
   return;
}
