#ifndef __SEMANTICS_H
#define __SEMANTICS_H

#include "general.h"
#include "error.h"
#include "symbol.h"
#include "intermediateCode.h"

// Stacks used for inheritted atributes in SDT shemas.
enum StackType { SYM_ENTRY, NEXT_LIST, LABEL_LIST };

typedef struct StackTag* Stack;
struct StackTag {
   union {
      SymbolEntry *p;
      labelListType *l;
      int *lbl; // TODO: remove this. most likely it's not needed
   } u;

   enum StackType type;
   struct StackTag *next;
};

Stack pop(Stack);
void* top(Stack);
Stack pushSymEntry(Stack, SymbolEntry*);
Stack pushList(Stack, labelListType*);
Stack pushLabel(Stack, int);

Const applyUnop(char, Const);
Const applyOperation(char, Const, Const);
void addConstant(char *, Type, Const);

Type arrayTypeCheck(Const, Type);
Type exprTypeCheck(char, Type, Type);
Type unopTypeCheck(char, Type);
bool arithmeticType(Type);
bool compatibleTypes(Type t1, Type t2);
bool assignmentCompatibleTypes(Type t1, Type t2);

rlvalue exprCodeGen(char, rlvalue, rlvalue);
rlvalue unopCodeGen(char,  rlvalue);
SymbolEntry *findLvaluePlace(lvalue);
rlvalue genCodeBooleanExpr(rlvalue, SymbolEntry*);

Stack paramCodeGen(Stack, Stack, rlvalue);

void addLibraryFunctions();

#endif
