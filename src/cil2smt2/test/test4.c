#include <stdio.h>

double msin(double x) {
    /*---------------------------- 2^-26 < |x|< 0.25 ----------------------*/
    double s1 = -0.16666666666666666;
    double s2 =  0.0083333333333323288;
    double s3 = -0.00019841269834414642;
    double s4 =  2.755729806860771e-06;
    double s5 = -2.5022014848318398e-08;
    double xx = x*x;
    /*Taylor series */
    double t = ((((s5*xx + s4)*xx + s3)*xx + s2)*xx + s1)*(xx*x);
    double res = x+t;
    double cor = (x-res)+t;
    return (res == res + 1.07 * cor) ? res : 99999;
}

int main() {
    printf("%.20g\n", msin(0.2000002811453019));
    return 0;
}
