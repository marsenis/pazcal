#include "symbol.h"

#ifndef __INTERCODE__
#define __INTERCODE__

#define MAX 10000
/* ------------------- Data structures ------------------- */
// TODO: retv is depricated and should be replaced by assignment to $$
enum opType { UNIT, ENDU, ARRAY, ASG, NEQ, LEQ, GEQ, IFB, JUMP, LABEL, JUMPL, CALL, PAR, RET, RETV };

enum optsType { CONST, VAR, LBL, PASS, EMPTY };

typedef union {
   SymbolEntry *constant;
   SymbolEntry *variable;
   int          label;
   PassMode     mode;
} contentType;

typedef struct {
   enum optsType type;
   contentType content;
} opts; //OPeraTS

typedef struct {
   enum opType op;
   opts x, y, z;
} immType;

typedef struct labelListType {
   int label;
   struct labelListType *nxt;
} labelListType;

extern immType immCode[MAX];
extern int immCurrentPos;

/* ------------------- Functions -------------------  */
int nextQuad();
void genQuad(enum opType op, opts x, opts y, opts z);
labelListType *emptyList();
labelListType *makeList(int x);
labelListType *mergeLists(labelListType *l1, labelListType *l2);
void backpatch(labelListType *l, int z);
void printImm();

typedef struct {
   SymbolEntry   *Place;
   Type          t;
   labelListType *Next, *True, *False;
} nonterm ;

#endif
