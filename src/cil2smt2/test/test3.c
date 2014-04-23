#include <stdio.h>

double msin(double x) {
    printf("x = %.20g\n", x);
    double big = 52776558133248.0;
    double mp1 = 1.5707963407039642;
    double mp2 = -1.3909067564377153e-08;
    double mp3 = -4.9789962505147994e-17;
    double hpinv = 0.63661977236758138;
    double toint = 6755399441055744.0;
    double t,y,xn,a,da,v;
    printf("x * hpinv = %.20g\n", x * hpinv);
    t   = (x * hpinv + toint);
    xn  = t - toint;
    v = t;
    y   = (x - xn * mp1) - xn * mp2;
    printf("t = %.20g\n", t);
    printf("xn = %.20g\n", xn);
    printf("v = %.20g\n", v);
    printf("y = %.20g\n", y);
    return y;
}

int main() {
    printf("%g\n", msin(2.56));
    return 0;
}
