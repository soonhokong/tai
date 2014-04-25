#pragma once

#define GET_LOW_NBITS(x, n) get_low_nbits(x, n, #x, __FILE__, __LINE__)
typedef union {int i[2]; double x;} mynumber;

int get_low_nbits(double x, unsigned n, char * s, char * filename, unsigned lineno);
