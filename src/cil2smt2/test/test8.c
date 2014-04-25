void f() {
    int n;
    double a;

    switch (n) {
    case 0:
    case 2:
        if (a * a < 0.01588) {
            a = 0.1;
        } else {
            a = -a;
        }
        break;
    case 1:
    case 3:
        if (a<0) {
            a = -a;
        }
        break;
    }
}
