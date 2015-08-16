#ifndef __SEMANTICS_H
#define __SEMANTICS_H

#include "general.h"
#include "error.h"
#include "symbol.h"
#include "intermediateCode.h"

// This extension allows the array size to also be a char constant
//#define EXT_POLYMORPHIC_ARRAY_SIZE

// TODO: replace those by variables so that you don't have
//       to perform a lookup each time you need them
#define SPACE   lookupEntry("$SPACE",   LOOKUP_ALL_SCOPES, true)
#define NEWLINE lookupEntry("$NEWLINE", LOOKUP_ALL_SCOPES, true)
#define ZERO    lookupEntry("$ZERO",    LOOKUP_ALL_SCOPES, true)
#define TRUE    lookupEntry("$TRUE",    LOOKUP_ALL_SCOPES, true)
#define FALSE   lookupEntry("$FALSE",   LOOKUP_ALL_SCOPES, true)

// Stacks used for inheritted atributes in SDT shemas.
enum StackType { SYM_ENTRY, NEXT_LIST };

typedef struct StackTag* Stack;
struct StackTag {
   union {
      SymbolEntry *p;
      labelListType *l;
   } u;

   enum StackType type;
   struct StackTag *next;
};

Stack pop(Stack);
void* top(Stack);
Stack pushSymEntry(Stack, SymbolEntry*);
Stack pushList(Stack, labelListType*);
Stack pushRange(Stack, Range);

Const applyUnop(char, Const);
Const applyOperation(char, Const, Const);
SymbolEntry* addConstant(char *, Type, Const);

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
