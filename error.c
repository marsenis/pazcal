/******************************************************************************
 *  CVS version:
 *     $Id: error.c,v 1.2 2004/05/05 22:00:08 nickie Exp $
 ******************************************************************************
 *
 *  C code file : error.c
 *  Project     : PCL Compiler
 *  Version     : 1.0 alpha
 *  Written by  : Nikolaos S. Papaspyrou (nickie@softlab.ntua.gr)
 *  Date        : May 14, 2003
 *  Description : Generic symbol table in C, simple error handler
 *
 *  Comments: (in Greek iso-8859-7)
 *  ---------
 *  ������ �������� �����������.
 *  ����� ������������ ��������� ��� ��������� �����������.
 *  ������ ����������� ������������ ��� �����������.
 *  ���������� ����������� ����������
 */


/* ---------------------------------------------------------------------
   ---------------------------- Header files ---------------------------
   --------------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "general.h"
#include "error.h"


/* ---------------------------------------------------------------------
   --------- ��������� ��� ����������� ��� �������� ��������� ----------
   --------------------------------------------------------------------- */

#define __COLORS

#define KNRM  "\x1B[0m"
#define KRED  "\x1B[31m"
#define KGRN  "\x1B[32m"
#define KYEL  "\x1B[33m"
#define KBLU  "\x1B[34m"
#define KMAG  "\x1B[35m"
#define KCYN  "\x1B[36m"
#define KWHT  "\x1B[37m"

void internal (const char * fmt, ...)
{
   va_list ap;

   va_start(ap, fmt);
   if (fmt[0] == '\r')
      fmt++;
   else
      fprintf(stderr, "%s:%d: ", filename, linecount);

   fprintf(stderr, "[Internal error] ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   va_end(ap);
   exit(1);
}

void fatal (const char * fmt, ...)
{
   va_list ap;

   va_start(ap, fmt);
   if (fmt[0] == '\r')
      fmt++;
   else
      fprintf(stderr, "%s:%d: ", filename, linecount);

#ifdef __COLORS
   fprintf(stderr, KRED "[fatal error] ");
#else
   fprintf(stderr, "[fatal error] ");
#endif

   vfprintf(stderr, fmt, ap);

#ifdef __COLORS
   fprintf(stderr, KNRM);
#endif

   fprintf(stderr, "\n");
   va_end(ap);
   exit(1);
}

void error (const char * fmt, ...)
{
   va_list ap;

   va_start(ap, fmt);

   if (fmt[0] == '\r')
      fmt++;
   else
      fprintf(stderr, "%s:%d: ", filename, linecount);

#ifdef __COLORS
   fprintf(stderr, KRED "[error] ");
#else
   fprintf(stderr, "[error] ");
#endif

   vfprintf(stderr, fmt, ap);

#ifdef __COLORS
   fprintf(stderr, KNRM);
#endif

   fprintf(stderr, "\n");
   va_end(ap);
}

void warning (const char * fmt, ...)
{
   va_list ap;

   va_start(ap, fmt);
   if (fmt[0] == '\r')
      fmt++;
   else
      fprintf(stderr, "%s:%d: ", filename, linecount);

#ifdef __COLORS
   fprintf(stderr, KYEL "[warning] ");
#else
   fprintf(stderr, "[warning] ");
#endif

   vfprintf(stderr, fmt, ap);

#ifdef __COLORS
   fprintf(stderr, KNRM);
#endif

   fprintf(stderr, "\n");
   va_end(ap);
}

void note(const char * fmt, ...)
{
   va_list ap;

   va_start(ap, fmt);
   if (fmt[0] == '\r')
      fmt++;
   else
      fprintf(stderr, "%s:%d: ", filename, linecount);

#ifdef __COLORS
   fprintf(stderr, KGRN "[note] ");
#else
   fprintf(stderr, "[note] ");
#endif

   vfprintf(stderr, fmt, ap);

#ifdef __COLORS
   fprintf(stderr, KNRM);
#endif

   fprintf(stderr, "\n");
   va_end(ap);
}
