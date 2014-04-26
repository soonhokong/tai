CFLAGS=-std=c99 -pedantic-errors
CC=gcc

all: test1 test2 test3 test4 test5 test6 test7 test8 test9

test1: main.c get_low_nbits.h get_low_nbits.c test1.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c test1.c

test2: main.c get_low_nbits.h get_low_nbits.c test2.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c test2.c

test3: main.c get_low_nbits.h get_low_nbits.c test3.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c test3.c

test4: main.c get_low_nbits.h get_low_nbits.c test4.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c test4.c

test5: main.c get_low_nbits.h get_low_nbits.c test5.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c test5.c

test6: main.c get_low_nbits.h get_low_nbits.c test6.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c test6.c

test7: main.c get_low_nbits.h get_low_nbits.c test7.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c test7.c

test8: main.c get_low_nbits.h get_low_nbits.c test8.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c test8.c

test9: main.c get_low_nbits.h get_low_nbits.c test9.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c test9.c

clean:
	-rm -rf test[0-9]
