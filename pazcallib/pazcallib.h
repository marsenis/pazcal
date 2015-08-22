#ifndef __PAZCALLIB_H
#define __PAZCALLIB_H

typedef char bool;
typedef double REAL;

void WRITE_INT(int, int);
void WRITE_BOOL(bool, int);
void WRITE_CHAR(char, int);
void WRITE_REAL(REAL, int, int);
void WRITE_STRING(char *, int);

int  READ_INT();
bool READ_BOOL();
REAL READ_REAL();
void READ_STRING(int, char *);

REAL arctan(REAL);
REAL ln(REAL);
REAL pi();

int TRUNC(REAL r);
int ROUND(REAL r);

#endif
