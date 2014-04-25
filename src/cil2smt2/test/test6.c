double f (double x) {
    double a[3] = {1.0, 2.0, 3.0};
    int b[3] = {0x000a, 0x0001, 0x1234};
    int i = 10;
    int z;
    double tmp1 = a[i];
    double tmp2 = b[i];
    double ret = tmp1 + tmp2 + x;
    return ret;
}
