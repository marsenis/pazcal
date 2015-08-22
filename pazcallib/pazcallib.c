#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "pazcallib.h"

/*
void exit(int exitcode)
{
   asm("syscall" :: "a" (60), "D" (exitcode));
}
*/

/*
size_t strlen(char s[])
{
   char *s_init = s;
   for (s_init = s; *s; s++) ;
   return s - s_init;
}
*/

void swap_char(char *c1, char *c2)
{
   char tmp = *c1;
   *c1 = *c2;
   *c2 = tmp;
}

void reverse(char buf[], int cnt) {
   int j;
   for (j = 0; 2*j < cnt; j++)
      swap_char(buf + j, buf + (cnt-1) - j);
}

void format_reverse(char buf[], int cnt, int w) {
   for (; cnt < w; cnt++)
      buf[cnt] = ' ';
   reverse(buf, cnt);
   buf[cnt] = '\0';
}

/*
void putchar(char c)
{
   write(1, &c, 1);

   ssize_t n;
   asm volatile (
      "movq $1, %%rdi\n" // 1st argument (int fd)
      "movq $1, %%rdx\n" // 3rd argument (size_t count)
      "movq $1, %%rax\n" // syscall number (1 for write)
      : "=A" (n)         // result in n
      : "S" (&c) );      // 2nd argument (const void *buf) in %rsi
}
*/

/*
void puts(char s[])
{
   write(1, s, strlen(s));

   size_t len = strlen(s);
   ssize_t n;

   asm volatile (
      "movq $1, %%rdi\n" // 1st argument (int fd)
      "movq %1, %%rdx\n" // 3rd argument (size_t count)
      "movq $1, %%rax\n" // syscall number (1 for write)
      "syscall"
      : "+A" (n)         // result in n
      : "r" (len),
        "S" (s) );       // 2nd argument (const void *buf) in %rsi
}
*/

// TODO: remove the constrain of 100 characters
void WRITE_INT(int n, int w)
{
   char buf[100];
   int cnt, j;

   for (cnt = 0; n; cnt++) {
      buf[cnt] = (char) (n % 10) + '0';
      n /= 10;
   }
   format_reverse(buf, cnt, w);

   fputs(buf, stdout);
}

void WRITE_BOOL(bool b, int w)
{
   char buf[100];

   strcpy(buf, b ? "eurt" : "eslaf");
   format_reverse(buf, 4 + !b, w);

   fputs(buf, stdout);
}

void WRITE_CHAR(char c, int w)
{
   int buf[100];

   buf[0] = c;
   format_reverse(buf, 1, w);

   fputs(buf, stdout);
}

void WRITE_REAL(REAL r, int w, int d) {
   int f[50];
   sprintf(f, "%%%d.%dlf", w, d);
   printf(f, r);
}

void WRITE_STRING(char s[], int w) {
   char buf[100];
   int cnt = strlen(s);

   strcpy(buf, s);
   reverse(buf, cnt);
   format_reverse(buf, cnt, w);

   fputs(buf, stdout);
}

int READ_INT()
{
   int res;
   scanf("%d", &res);
   return res;
}

bool READ_BOOL()
{
   /* TODO */
}

REAL READ_REAL()
{
   REAL res;
   scanf("%lf", &res);
   return res;
}

void READ_STRING(int size, char s[])
{
   fgets(s, size, stdin);
}

REAL arctan(REAL r)
{
   return atan(r);
}

REAL ln(REAL r)
{
   return log(r);
}

REAL pi()
{
   return M_PI;
}

int TRUNC(REAL r)
{
   return (int) trunc(r);
}

int ROUND(REAL r)
{
   return (int) round(r);
}
