#include "symbol.h"

#ifndef __INTERCODE__
#define __INTERCODE__

#define MAX 10000

#define EMT ((opts) { EMPTY, {0} })  // Empty Quad
#define Var(X)  ( (opts) { VAR  ,   (contentType) { .variable = (X) } } )
#define Cnst(X) ( (opts) { CONST,   (contentType) { .constant = (X) } } )
#define Mode(X) ( (opts) { PASS,    (contentType) { .mode = (X) } } )
#define Lbl(X)  ( (opts) { LBL,     (contentType) { .label  = (X) } } )
#define Ref(X)  ( (opts) { REF_VAR, (contentType) { .variable = (X) } } )

/* ------------------- Data structures ------------------- */
// TODO: retv is depricated and should be replaced by assignment to $$
enum opType { UNIT, ENDU, ARRAY, ASG, NEQ, LEQ, GEQ, IFB, JUMP, LABEL, JUMPL, CALL, PAR, RET, RETV, OP_PLUS = '+', OP_MINUS = '-', OP_TIMES = '*', OP_DIV = '/', OP_MOD = '%', OP_EQ = '=', OP_LESS = '<', OP_LEQ = ',', OP_GR = '>', OP_GEQ = '.', OP_NOT = '!' };

enum optsType { CONST, VAR, LBL, PASS, REF_VAR, EMPTY };

typedef union {
   SymbolEntry *variable;
   int          constant;
   int          label;
   PassMode     mode;
} contentType;

typedef struct {
   enum optsType type;
   contentType content;
   Scope *scope; // Temporary: It's used by UNIT to hold the new scope
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
} rlvalue ;

// Dragon Book, page 384
typedef struct {
   SymbolEntry *addr;
   SymbolEntry *array;
   Type        type;
} lvalue;

typedef struct {
   rlvalue      from, to, step;
   int          direction;
} Range;

typedef struct {
   SymbolEntry   *t1, *t2, *t3, *t4;
   labelListType *Next;
   int            LoopBeg;
} loopContext;
   
#endif
