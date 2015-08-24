.PHONY: clean, all

CC=gcc
GAS=as
CLIBFLAGS=-w -fverbose-asm -fno-stack-protector
CFLAGS=-w -O2
LDFLAGS=-lc --entry main

all: target test pazcallib.s pazcallib.o

pazcallib.o: pazcallib.s
	$(GAS) -o $@ $<

pazcallib.s: pazcallib.c pazcallib.h
	$(CC) $(CLIBFLAGS) -S -m64 -o $@  $<

test.s: test.c pazcallib.h
	$(CC) $(CFLAGS) -S -o $@ $<

test.o: test.s
	$(GAS) -o $@ $<

#test.o: test.c pazcallib.h
#	$(CC) $(CFLAGS) -c -o $@ $<

#test: test.o pazcallib.o
#	$(LD) $(LDFLAGS) -o $@ $^

test: test.o pazcallib.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

target.o: target.s
	$(GAS) -o $@ $<

target: target.o pazcallib.o
	$(CC) $(CFLAGS) -o $@ $^ -lm
	#$(LD) $(LDFLAGS) -o $@ $^

clean:
	$(RM) pazcallib.o pazcallib.s test test.o test.s target target.o
