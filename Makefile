CC=gcc
CFLAGS=-g -Wall -Wno-switch -Wno-unused-function

pazcal: pazcal.lex.o pazcal.tab.o error.o general.o symbol.o symbolDebug.o semantics.o intermediateCode.o target.o
		$(CC) $(CFLAGS) -o pazcal $+ -lfl

pazcal.lex.c: pazcal.l
		flex -s --header-file=pazcal.lex.h -o pazcal.lex.c pazcal.l

pazcal.tab.c pazcal.tab.h: pazcal.y
		bison -dv pazcal.y

%.o: %.c pazcal.tab.h error.h general.h symbol.h semantics.h intermediateCode.h
		$(CC) $(CFLAGS) -c $<

clean:
		$(RM) *.o pazcal.lex.c pazcal.tab.c pazcal.tab.h pazcal.output pazcal.lex.h

distclean: clean
		$(RM) pazcal
