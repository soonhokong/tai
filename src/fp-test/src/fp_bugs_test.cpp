#include <cmath>
#include <cfloat>
#include <cstring>
#include <fenv.h>
#include <functional>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <tuple>
#include <utility>
#include <vector>
#include <bitset>
#include <ieee754.h>
#include <mpfr.h>
#include "isnan.h"
#include "isinf.h"

using std::cerr;
using std::cout;
using std::endl;
using std::function;
using std::get;
using std::make_pair;
using std::make_tuple;
using std::ostream;
using std::pair;
using std::setprecision;
using std::setw;
using std::string;
using std::tuple;
using std::vector;
using std::bitset;
using std::stringstream;

class C {
public:
    C(unsigned p = 256) {
        mpfr_init2(mpfr_tmp, p);
    }
    ~C() {
        mpfr_clear(mpfr_tmp);
    }
    template<typename F, typename MF>
    tuple<double, double, double> compute(double const x, F std_f, MF mpfr_f, tuple<string, int, mpfr_rnd_t> const & rnd) {
        double std_result, mpfr_result, mpfr_result_rndn;
        int fe_rnd = get<1>(rnd);
        mpfr_rnd_t mpfr_rnd = get<2>(rnd);
        int fe_old_rnd = fegetround();
        // std_result
        fesetround(fe_rnd);
        std_result = std_f(x);
        fesetround(fe_old_rnd);
        // mpfr_result
        mpfr_set_d(mpfr_tmp, x, MPFR_RNDN);
        mpfr_f(mpfr_tmp, mpfr_tmp, mpfr_rnd);
        mpfr_result = mpfr_get_d(mpfr_tmp, mpfr_rnd);
        // mpfr_result_rndn
        mpfr_set_d(mpfr_tmp, x, MPFR_RNDN);
        mpfr_f(mpfr_tmp, mpfr_tmp, MPFR_RNDN);
        mpfr_result_rndn = mpfr_get_d(mpfr_tmp, MPFR_RNDN);
        return make_tuple(std_result, mpfr_result, mpfr_result_rndn);
    }
private:
    mpfr_t mpfr_tmp;
};

double fRand(double const fMin, double const fMax) {
    double f = (double)rand() / RAND_MAX;
    return fMin + f * (fMax - fMin);
}

unsigned long fpmantissa(double const number)
{
    ieee754_double x = {.d = number};
    return ((unsigned long) x.ieee.mantissa0 << 32) + x.ieee.mantissa1;
}

int fpexponent(double const number)
{
    ieee754_double x = {.d = number};
    return x.ieee.exponent - 1023;
}

string get_binary_rep(double x) {
    stringstream ss;
    if (check_nan(x)) {
        return "nan";
    } else if (check_inf(x)) {
        return (x > 0) ? "+oo" : "-oo" ;
    }
    bool          s = (x >= 0);
    unsigned long m = fpmantissa(x);
    int           e = fpexponent(x);
    if (s) {
        ss << "+";
    } else {
        ss << "-";
    }
    ss << "1." << bitset<52>(m) << " x 2^" << e;
    return ss.str();
}

string get_binary_diff(double const x, double const y) {
    bitset<52> x_m = fpmantissa(x);
    bitset<52> y_m = fpmantissa(y);
    return (x_m ^ y_m).to_string();
}

int fpcmp(double const x, double const y) {
    if (x >= 0 && y < 0) {
        return 1;
    }
    if (y >= 0 && x < 0) {
        return -1;
    }
    int const x_e = fpexponent(x);
    int const y_e = fpexponent(y);
    unsigned long const x_m = fpmantissa(x);
    unsigned long const y_m = fpmantissa(y);
    int s = (x >= 0) ? +1 : -1;

    if(x_e > y_e) {
        return s * 1;
    } else if (y_e > x_e) {
        return s * -1;
    } else if (x_m > y_m) {
        return s * 1;
    } else if (y_m > x_m) {
        return s * -1;
    }
    return 0;
}

bool isConsistent(double const x, double const y, unsigned long const ulp) {
    if (x == y)
        return true;
    if (check_nan(x) || check_nan(y))
        return false;
    if (check_inf(x) || check_inf(y))
        return false;
    if (x >= 0 && y < 0)
        return false;
    if (x <= 0 && y > 0)
        return false;
    int x_e = fpexponent(x);
    int y_e = fpexponent(y);
    int e_diff = abs(x_e - y_e);
    if (e_diff > 3) {
        return false;
    }
    unsigned long x_m = fpmantissa(x);
    unsigned long y_m = fpmantissa(y);
    if (x_e > y_e) {
        x_m = x_m << e_diff;
    } else {
        y_m = y_m << e_diff;
    }
    if ((x_m > y_m + ulp) || (y_m > x_m + ulp)) {
        return false;
    }
    return true;
}

bool isCorrectRnd(double const x, double const ref, int rnd) {
    if (x >= 0.0 && ref < 0.0)
        return false;
    if (x <= 0.0 && ref > 0.0)
        return false;

    switch(rnd) {
    case FE_TONEAREST:
        return true;
    case FE_UPWARD:
        return fpcmp(x, ref) == 0;
    case FE_DOWNWARD:
        return fpcmp(x, ref) == 0;
    case FE_TOWARDZERO:
        return (x >= 0.0 && fpcmp(x, ref) <= 0.0) || (x <= 0.0 && fpcmp(x, ref) >= 0.0);
    }
    return false;
}

ostream & display_instance(ostream & out, string const f, double const x,
                           double const r1, double const r2, double const , string const rnd) {
    out << f << "(" << setprecision(30) << x << ") = " << setprecision(30) << r1 << "\t" << rnd << endl;
    out << setw(25) << " = " << setprecision(30) << r2 << "\t" << rnd << endl;
//    out << setw(25) << " = " << setprecision(30) << r3 << "\t" << "RNDN" << endl;

    out << "std  = "    << get_binary_rep(r1) << endl;
    out << "mpfr = "    << get_binary_rep(r2) << endl;
    out << "diff =    " << get_binary_diff(r1, r2) << endl;
    out << endl;
    return out;
}

ostream & display_stat_header(ostream & out) {
    out << setw(15) << "Function"     << setw(15) << "Round" << setw(15) << "Inconsistent"
        << setw(15) << "Incorrect" << setw(15) << "Infty" << setw(15) << "Nan"
        << setw(15) << "Total"        << endl;
    return out;
}

ostream & display_stat_row(ostream & out, tuple<string, string, unsigned, unsigned, unsigned, unsigned> row) {
    unsigned total = get<2>(row) + get<3>(row) + get<4>(row) + get<5>(row);
    out << setw(15) << get<0>(row) << setw(15) << get<1>(row)
        << setw(15) << get<2>(row) << setw(15) << get<3>(row)
        << setw(15) << get<4>(row) << setw(15) << get<5>(row)
        << setw(15) << total       << endl;
    return out;
}

void ReplaceStringInPlace(std::string& subject, const std::string& search,
                          const std::string& replace) {
    size_t pos = 0;
    while ((pos = subject.find(search, pos)) != std::string::npos) {
        subject.replace(pos, search.length(), replace);
        pos += replace.length();
    }
}

ostream & display_stat_row_latex(ostream & out, tuple<string, string, unsigned, unsigned, unsigned, unsigned> row) {
    unsigned total = get<2>(row) + get<3>(row) + get<4>(row) + get<5>(row);
    string func = get<0>(row);
    string rnd = get<1>(row);
    ReplaceStringInPlace(func, "_", "\\_");
    ReplaceStringInPlace(rnd,  "_", "\\_");
    out << setw(15) << func        << "&" << setw(15) << rnd         << "&"
        << setw(15) << get<2>(row) << "&" << setw(15) << get<3>(row) << "&"
        << setw(15) << get<4>(row) << "&" << setw(15) << get<5>(row) << "&"
        << setw(15) << total       << "\\\\" << endl;
    return out;
}

int main() {
    srand(time(NULL));
    C helper(256);

    // Rounding Modes
    vector<tuple<string, int, mpfr_rnd_t>> rnds =
        {make_tuple("NEAREST",  FE_TONEAREST,  MPFR_RNDN),
         make_tuple("UPWARD",   FE_UPWARD,     MPFR_RNDU),
         make_tuple("DOWNWARD", FE_DOWNWARD,   MPFR_RNDD),
         make_tuple("TO_ZERO",  FE_TOWARDZERO, MPFR_RNDZ)};
    vector<tuple<string, function<double(double)>, function<int(mpfr_t, mpfr_t, mpfr_rnd_t)>,
                 double, double, double, double> > funcs =
        //       FuncName    C_FUN  MPFR_FUN    DOM_L    DOM_U   RANGE_L   RANGE_U
        {make_tuple("sin",   sin,   mpfr_sin,  -1e306, +1e306, -1.00,     +1.00),
         make_tuple("cos",   cos,   mpfr_cos,  -1e306, +1e306, -1.00,     +1.00),
         make_tuple("tan",   tan,   mpfr_tan,  -1e306, +1e306, -INFINITY, +INFINITY),
         make_tuple("acos",  acos,  mpfr_acos, -1.00,  +1.00,  -INFINITY, +INFINITY),
         make_tuple("asin",  asin,  mpfr_asin, -1.00,  +1.00,  -INFINITY, +INFINITY),
         make_tuple("atan",  atan,  mpfr_atan, -1.00,  +1.00,  -INFINITY, +INFINITY),
         make_tuple("cosh",  cosh,  mpfr_cosh, -500,   +500,    1.0,      +INFINITY),
         make_tuple("sinh",  sinh,  mpfr_sinh, -500,   +500,   -INFINITY, +INFINITY),
         make_tuple("tanh",  tanh,  mpfr_tanh, -20,    +20,    -1.0,      +1.0),
         make_tuple("exp",   exp,   mpfr_exp,  -100,   +100,    0.0,      +INFINITY),
         make_tuple("log",   log,   mpfr_log,  1e-306, +1e306, -INFINITY, +INFINITY),
         make_tuple("log10", log10, mpfr_log10,1e-306, +1e306, -INFINITY, +INFINITY),
         make_tuple("sqrt",  sqrt,  mpfr_sqrt,  0.00,  +1e306,  0.0,      +INFINITY),
        };
    double std_result, mpfr_result, mpfr_result_rndn, /*eps,*/ x;
    unsigned const max_iter = 100000;
    unsigned const ulp_threshold = 1;

    tuple<double, double, double> result;

    vector<tuple<string, string, unsigned, unsigned, unsigned, unsigned>> stat;

    for (auto const & func : funcs) {
        for (auto const & rnd : rnds) {
            unsigned inconsistent = 0, nan = 0, inf = 0, incorrect = 0;
            for (unsigned i = 0; i < max_iter; i++) {
                x = fRand(get<3>(func), get<4>(func));
                result = helper.compute(x, get<1>(func), get<2>(func), rnd);
                std_result       = get<0>(result);
                mpfr_result      = get<1>(result);
                mpfr_result_rndn = get<2>(result);

                if (!isConsistent(mpfr_result, std_result, ulp_threshold)) {
//                    display_instance(cerr, get<0>(func), x, std_result, mpfr_result, mpfr_result_rndn, get<0>(rnd));
                    if (check_nan(std_result)) { nan++; }
                    else if (check_inf(std_result)) { inf++; }
                    else if (std_result < get<5>(func) || get<6>(func) < std_result) { incorrect++; }
                    else { inconsistent++; }
                    if (!isCorrectRnd(std_result, mpfr_result, get<1>(rnd))) {
                        display_instance(cerr, get<0>(func), x, std_result, mpfr_result, mpfr_result_rndn, get<0>(rnd));
                    }
                }

            }
            unsigned total = inconsistent + incorrect + inf + nan;
            if (total > 0) {
                stat.emplace_back(get<0>(func), get<0>(rnd), inconsistent, incorrect, inf, nan);
            }
        }
    }
    display_stat_header(cout);
    for (auto const & row : stat) {
        // display_stat_row_latex(cout, row);
        display_stat_row(cout, row);
    }
    return 0;
}
