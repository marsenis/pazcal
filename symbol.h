/******************************************************************************
 *  CVS version:
 *     $Id: symbol.h,v 1.1 2003/05/13 22:21:01 nickie Exp $
 ******************************************************************************
 *
 *  C header file : symbol.h
 *  Project       : PCL Compiler
 *  Version       : 1.0 alpha
 *  Written by    : Nikolaos S. Papaspyrou (nickie@softlab.ntua.gr)
 *  Date          : May 14, 2003
 *  Description   : Generic symbol table in C
 *
 *  Comments: (in Greek iso-8859-7)
 *  ---------
 *  ������ �������� �����������.
 *  ����� ������������ ��������� ��� ��������� �����������.
 *  ������ ����������� ������������ ��� �����������.
 *  ���������� ����������� ����������
 */


#ifndef __SYMBOL_H__
#define __SYMBOL_H__

//#define DEBUG_SYMBOL

/* ---------------------------------------------------------------------
   -------------------------- ����� bool -------------------------------
   --------------------------------------------------------------------- */

#include <stdbool.h>

/*
 *  �� �� �������� include ��� ������������� ��� ��� ���������
 *  ��� C ��� ��������������, �������������� �� �� �� ��������:
 */

#if 0
typedef enum { false=0, true=1 } bool;
#endif


/* ---------------------------------------------------------------------
   ------------ ������� �������� ��� ������ �������� -------------------
   --------------------------------------------------------------------- */

#define START_POSITIVE_OFFSET 16//8     /* ������ ������ offset ��� �.�.   */
#define START_NEGATIVE_OFFSET 0     /* ������ �������� offset ��� �.�. */


/* ---------------------------------------------------------------------
   --------------- ������� ����� ��� ������ �������� -------------------
   --------------------------------------------------------------------- */

/* ����� ��������� ��� ��� ��������� ��� �������� */

typedef int           RepInteger;         /* ��������                  */
typedef unsigned char RepBoolean;         /* ������� �����             */
typedef char          RepChar;            /* ����������                */
/* Changed it from 'long double' to 'double' because of ABI warnings
 * TODO: consider changing it back */
typedef double   RepReal;            /* �����������               */
/* Initially it was 'const char'
 * TODO: put back the 'const' if needed */
typedef char *  RepString;          /* �������������             */


/* ����� ��������� ��� ������������� ����������� */

typedef struct Type_tag * Type;

struct Type_tag {
    enum {                               /***** �� ����� ��� ����� ****/
       TYPE_VOID,                        /* ����� ����� ������������� */
       TYPE_INTEGER,                     /* ��������                  */
       TYPE_BOOLEAN,                     /* ������� �����             */
       TYPE_CHAR,                        /* ����������                */
       TYPE_REAL,                        /* �����������               */
       TYPE_ARRAY,                       /* ������� ������� ��������  */
       TYPE_IARRAY,                      /* ������� �������� �������� */
       TYPE_POINTER                      /* �������                   */
    } kind;
    Type           refType;              /* ����� ��������            */
    RepInteger     size;                 /* �������, �� ����� ������� */
    unsigned int   refCount;             /* �������� ��������         */
};


/* ����� �������� ��� ������ �������� */

typedef enum {            
   ENTRY_VARIABLE,                       /* ����������                 */
   ENTRY_CONSTANT,                       /* ��������                   */
   ENTRY_FUNCTION,                       /* �����������                */
   ENTRY_PARAMETER,                      /* ���������� �����������     */
   ENTRY_TEMPORARY                       /* ���������� ����������      */
} EntryType;


/* ����� ���������� ���������� */

typedef enum {            
   PASS_BY_VALUE,                        /* ���' ����                  */
   PASS_BY_REFERENCE,                    /* ���' �������               */
   PASS_RET                              /* added by marsenis for intermediate code */
} PassMode;

// Added by marsenis.
typedef union {                              /* ����                  */
   RepInteger vInteger;              /*    �������            */
   RepBoolean vBoolean;              /*    ������             */
   RepChar    vChar;                 /*    ����������         */
   RepReal    vReal;                 /*    ����������         */
   RepString  vString;               /*    ������������       */
} RepTypes;
typedef struct {                        /******** ������� ********/
   Type          type;                  /* �����                 */
   RepTypes      value;
   int           id;                    /* Used in target code generation */
} Const;

/* ����� �������� ���� ������ �������� */

typedef struct SymbolEntry_tag SymbolEntry;

struct SymbolEntry_tag {
   const char   * id;                 /* ����� ��������������          */
   EntryType      entryType;          /* ����� ��� ��������            */
   unsigned int   nestingLevel;       /* ����� �����������             */
   unsigned int   hashValue;          /* ���� ���������������          */
   SymbolEntry  * nextHash;           /* ������� ������� ���� �.�.     */
   SymbolEntry  * nextInScope;        /* ������� ������� ���� �������� */

   union {                            /* ������� �� ��� ���� ��������: */

      struct {                                /******* ��������� *******/
         Type          type;                  /* �����                 */
         int           offset;                /* Offset ��� �.�.       */
      } eVariable;

      // Modified by marsenis <marsenis@gmail.com>
      // It is more convenient to have a common
      // data type for Constants in the symbol table
      // and the main program during the evaluation
      // of constants at compilation time
      Const eConstant;
      //struct {                                /******** ������� ********/
      //   Type          type;                  /* �����                 */
      //   
      //   union {                              /* ����                  */
      //      RepInteger vInteger;              /*    �������            */
      //      RepBoolean vBoolean;              /*    ������             */
      //      RepChar    vChar;                 /*    ����������         */
      //      RepReal    vReal;                 /*    ����������         */
      //      RepString  vString;               /*    ������������       */
      //   } value;
      //} eConstant;

      struct {                                /******* ��������� *******/
         bool          isForward;             /* ������ forward        */
         SymbolEntry * firstArgument;         /* ����� ����������      */
         SymbolEntry * lastArgument;          /* ��������� ����������  */
         Type          resultType;            /* ����� �������������   */
         enum {                               /* ��������� ����������  */
             PARDEF_COMPLETE,                    /* ������ �������     */
             PARDEF_DEFINE,                      /* �� ���� �������    */
             PARDEF_CHECK                        /* �� ���� �������    */
         } pardef;
         int           firstQuad;             /* ������ �������        */
      } eFunction;

      struct {                                /****** ���������� *******/
         Type          type;                  /* �����                 */
         int           offset;                /* Offset ��� �.�.       */
         int           position;              /* it's the "position"-th parameter */
         PassMode      mode;                  /* ������ ����������     */
         SymbolEntry * next;                  /* ������� ����������    */
      } eParameter;

      struct {                                /** ��������� ��������� **/
         Type          type;                  /* �����                 */
         int           offset;                /* Offset ��� �.�.       */
         int           number;
      } eTemporary;

   } u;                               /* ����� ��� union               */
};


/* ����� ������� �������� ��� ���������� ���� ���� �������� */

typedef struct Scope_tag Scope;

struct Scope_tag {
    unsigned int   nestingLevel;             /* ����� �����������      */
    unsigned int   negOffset;                /* ������ �������� offset */
    Scope        * parent;                   /* ������������ ��������  */
    SymbolEntry  * entries;                  /* ������� ��� ���������  */
};


/* ����� ���������� ���� ������ �������� */

typedef enum {
    LOOKUP_CURRENT_SCOPE,
    LOOKUP_ALL_SCOPES
} LookupType;


/* ---------------------------------------------------------------------
   ------------- ��������� ���������� ��� ������ �������� --------------
   --------------------------------------------------------------------- */

extern Scope        * currentScope;       /* �������� ��������         */
extern unsigned int   quadNext;           /* ������� �������� �������� */
extern unsigned int   tempNumber;         /* �������� ��� temporaries  */

extern const Type typeVoid;
extern const Type typeInteger;
extern const Type typeBoolean;
extern const Type typeChar;
extern const Type typeReal;


/* ---------------------------------------------------------------------
   ------ ��������� ��� ����������� ��������� ��� ������ �������� ------
   --------------------------------------------------------------------- */

void          initSymbolTable    (unsigned int size);
void          destroySymbolTable (void);

void          openScope          (void);
void          closeScope         (void);

SymbolEntry * newVariable        (const char * name, Type type);
SymbolEntry * newConstant        (const char * name, Type type, ...);
SymbolEntry * newFunction        (const char * name);
SymbolEntry * newParameter       (const char * name, Type type,
                                  PassMode mode, SymbolEntry * f);
SymbolEntry * newTemporary       (Type type);

void          forwardFunction    (SymbolEntry * f);
void          endFunctionHeader  (SymbolEntry * f, Type type);
void          destroyEntry       (SymbolEntry * e);
SymbolEntry * lookupEntry        (const char * name, LookupType type,
                                  bool err);

Type          typeArray          (RepInteger size, Type refType);
Type          typeIArray         (Type refType);
Type          typePointer        (Type refType);
void          destroyType        (Type type);
unsigned int  sizeOfType         (Type type, bool fullArray);
bool          equalType          (Type type1, Type type2);
void          printType          (Type type);
void          printMode          (PassMode mode);
Type          getSymType         (SymbolEntry *s);

// Added by marsenis
char* newConstName();
/* Added for debugging. It's defined in symbolDebug.c */
void          printSymbolTable   ();
void          printMismatch      ();

#endif
