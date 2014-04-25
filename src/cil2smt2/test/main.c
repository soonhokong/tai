#include <stdio.h>
#include <stdlib.h>
#include "get_low_nbits.h"
double f(double x);

int main (int argc, char *argv[])
{
    if(argc != 2) {
        printf("usage: %s <double number>\n", argv[0]);
        return 1;
    }
    double arg = atof(argv[1]);
    printf("%g\n", f(arg));
    return 0;
}
