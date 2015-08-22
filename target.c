#include "target.h"
#include "intermediateCode.h"
#include "symbol.h"
#include "general.h"
#include "error.h"

void generateTarget(FILE *f) {
   int i;

   // Preamble
   printf("\t.file \"%s\"\n", filename);
   printf("\t.section .rodata\n");

   // Constant Strings
    Scope       * scp;
    SymbolEntry * e;
    SymbolEntry * args;
    
    scp = currentScope;
    while (scp != NULL) {
        e = scp->entries;
        while (e != NULL) {
            if (e->entryType == ENTRY_CONSTANT
                && equalType(e->u.eConstant.type, typeArray)
                && equalType(e->u.eConstant.type.refType, typeChar) )
               printf(".LC...");


   /*
   for (i = 0; i < immCurrentPos; i++) {
   }
   */
}
