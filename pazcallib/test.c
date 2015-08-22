#include "pazcallib.h"

int main() {
   WRITE_INT(42, 11);
   putchar('\n');
   WRITE_BOOL(1, 11);
   putchar('\n');
   WRITE_STRING("makis", 11);
   putchar('\n');
   WRITE_REAL(pi(), 11, 2);
   puts("\nHello World");
   exit(0);
   return 0;
}
