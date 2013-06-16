#ifndef __SEMANTICS_H
#define __SEMANTICS_H

#include "general.h"
#include "error.h"
#include "symbol.h"
#include "intermediateCode.h"

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
bool arithmeticType(Type);
bool compatibleTypes(Type t1, Type t2);
bool assignmentCompatibleTypes(Type t1, Type t2);

nonterm exprCodeGen(char, nonterm, nonterm);

Stack paramCheck(Stack, Stack, Type);

void addLibraryFunctions();

#endif
