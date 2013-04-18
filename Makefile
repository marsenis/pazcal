CC=gcc
CFLAGS=-Wall -g

pazcal: pazcal.lex.o pazcal.tab.o error.o
		$(CC) $(CFLAGS) -o pazcal $+ -lfl

pazcal.lex.c: pazcal.l
		flex -s -o pazcal.lex.c pazcal.l

pazcal.tab.c pazcal.tab.h: pazcal.y
		bison -dv pazcal.y

%.o: %.c pazcal.tab.h error.h general.h
		$(CC) $(CFLAGS) -c $<

clean:
		$(RM) *.o pazcal.lex.c pazcal.tab.c pazcal.tab.h pazcal.output

distclean: clean
		$(RM) pazcal
