#ifndef __SEMANTICS_H
#define __SEMANTICS_H

#include "general.h"
#include "error.h"
#include "symbol.h"

// Stacks used for inheritted atributes in SDT shemas.
typedef struct StackTag* Stack;
struct StackTag {
   SymbolEntry *p;
   struct StackTag *next;
};

Stack pop(Stack);
SymbolEntry* top(Stack);
Stack push(Stack, SymbolEntry*);

Const applyUnop(char, Const);
Const applyOperation(char, Const, Const);
void addConstant(char *, Type, Const);

Type arrayTypeCheck(Const, Type);
Type exprTypeCheck(char, Type, Type);
Type unopTypeCheck(char, Type);
char aritheticType(Type);

Stack paramCheck(Stack, Stack, Type);

void addLibraryFunctions();

#endif
