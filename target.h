#ifndef __TARGET_H
#define __TARGET_H

/* Public: */
int newStringID();
void TG_Preamble();
void TG_StringConst(char *s, int);
void TG_Generate();

/* Private: */
/*
void label(int i);
enum dataTypes trans(Type t);
char *reg(enum regs R, enum dataTypes t);
char cmd(enum dataTypes t);

void load(enum regs R, opts x);
*/

#endif
