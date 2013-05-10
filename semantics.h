#ifndef __SEMANTICS_H
#define __SEMANTICS_H

#include "symbol.h"

typedef struct StackTag* Stack;
struct StackTag {
   SymbolEntry *p;
   struct StackTag *next;
};

Stack pop(Stack);
SymbolEntry* top(Stack);
Stack push(Stack, SymbolEntry*);

Const applyOperation(char, Const, Const);
void addConstant(char *, Type, Const);

Type arrayTypeCheck(Const, Type);
Type exprTypeCheck(char, Type, Type);
Type unopTypeCheck(char, Type);

Stack paramCheck(Stack, Stack, Type);

void addLibraryFunctions();

#endif
