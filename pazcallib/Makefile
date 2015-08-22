.PHONY: clean, all

CC=gcc
GAS=as
CLIBFLAGS=-w -O2 -fverbose-asm -fno-stack-protector
CFLAGS=-w -O2
#LDFLAGS=-lc --entry main -lm

all: test pazcallib.s pazcallib.o

pazcallib.o: pazcallib.s
	$(GAS) -o $@ $<

pazcallib.s: pazcallib.c pazcallib.h
	$(CC) $(CLIBFLAGS) -S -m64 -o $@  $<

test.o: test.c pazcallib.h
	$(CC) $(CFLAGS) -c -o $@ $<

#test: test.o pazcallib.o
#	$(LD) $(LDFLAGS) -o $@ $^

test: test.o pazcallib.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

clean:
	$(RM) pazcallib.o pazcallib.s test test.o
