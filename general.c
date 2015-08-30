/******************************************************************************
 *  CVS version:
 *     $Id: general.c,v 1.1 2004/05/05 22:00:08 nickie Exp $
 ******************************************************************************
 *
 *  C code file : general.c
 *  Project     : PCL Compiler
 *  Version     : 1.0 alpha
 *  Written by  : Nikolaos S. Papaspyrou (nickie@softlab.ntua.gr)
 *  Date        : May 5, 2004
 *  Description : Generic symbol table in C, general variables and functions
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

#include <stdlib.h>
#include <string.h>

#include "general.h"
#include "error.h"


/* ---------------------------------------------------------------------
   ----------- ��������� ��� ����������� ����������� ������ ------------
   --------------------------------------------------------------------- */

void * new (size_t size)
{
   void * result = malloc(size);
   
   if (result == NULL)
      fatal("\rOut of memory");
   return result;
}

void delete (void * p)
{
   if (p != NULL)
      free(p);
}

void rm_filename_ext(const char *filename, char *base) {
   const char *dot = strrchr(filename, '.');
   if(!dot || dot == filename) {
      strcpy(base, filename);
      return;
   }
   strncpy(base, filename, dot - filename);
   base[dot - filename] = '\0';
}


/* ---------------------------------------------------------------------
   ------- ������ ������� ��� ������������� ��� ������� ������� --------
   --------------------------------------------------------------------- */

const char * filename;
FILE *immfile;
FILE *asmfile;
int linecount;
