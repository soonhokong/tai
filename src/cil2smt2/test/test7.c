#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

double f(double k) {
    double array[2] = {4.0, 5.0};
    int x = 0;
    int y = 1;
    double tmp1 = array[x];
    double tmp2 = array[y];
    double ret = tmp1 + tmp2;
    return k + ret;
}

int main (int argc, char *argv[])
{
    assert(argc == 2);
    double arg = atof(argv[1]);
    printf("%g\n", f(arg));
    return 0;
}
