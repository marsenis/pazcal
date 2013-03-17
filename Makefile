CC=gcc
CFLAGS=-O2 -Wall -g

pazcal: pazcal.lex.o
		  $(CC) $(CFLAGS) -o pazcal pazcal.lex.o -lfl

pazcal.lex.c: pazcal.l
		  flex -s -o pazcal.lex.c pazcal.l

clean:
		 $(RM) *.o pazcal.lex.c pazcal
