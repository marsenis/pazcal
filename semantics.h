#ifndef __SEMANTICS_H
#define __SEMANTICS_H

#include "symbol.h"

Const applyOperation(char, Const, Const);
void addConstant(char *, Type, Const);

Type arrayTypeCheck(Const, Type);
Type exprTypeCheck(char, Type, Type);
Type unopTypeCheck(char, Type);

void addLibraryFunctions();

#endif
