#include <stdio.h>
#include "get_low_nbits.h"

int get_low_nbits(double x, unsigned n, char * s, char * filename, unsigned lineno) {
    mynumber tmp;
    tmp.x = x;
    int high = tmp.i[1];
    int low  = tmp.i[0];
    int exp  = ((high >> 20) & ((1 << 11) - 1)) - 1023;
    int shigh = high & 0x000fffff;
    int slow  = low;
    int ret = slow;
    if (n < 32) {
        ret = ret & ((1 << n) - 1);
    }
    printf("%s|%d|", filename, lineno);
    printf("GET_LOW_NBITS|%s %d|", s, n);
    printf("%.30g %s %08x %08x %d|",
           x, (x >= 0 ? "+" : "-"), shigh, slow, exp);
    printf("%x\n", ret);
    return ret;
}
