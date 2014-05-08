CFLAGS=-std=c99 -pedantic-errors
CC=gcc

TEST_FILES=$(wildcard test*.c)
TESTS=$(TEST_FILES:.c=)

all: $(TESTS)

test%: test%.c main.c get_low_nbits.h get_low_nbits.c
	$(CC) $(CFLAGS) -o $@ main.c get_low_nbits.c $@.c

clean:
	-find . -perm +100 -type f -name "test*" -delete
