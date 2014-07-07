#!/usr/bin/env python
"""
Main Loop for Test-and-Infer
"""

__author__ = "Soonho Kong"
__copyright__ = "Copyright 2014, Soonho Kong"
__credits__ = ["Soonho Kong", "Sicun Gao", "Wei Chen", "Edmund Clarke"]
__license__ = "GPL"
__version__ = "0.1"
__maintainer__ = "Soonho Kong"
__email__ = "soonhok@cs.cmu.edu"
__status__ = "Production"

import argparse
import logging
import os
import math

from util import test, check_file_or_die, to_interval, read_model, print_model, get_filename, run_test, generate_SMT, run_dReal, is_SAT

# =================
# Argument Parsing
# =================
parser = argparse.ArgumentParser(description='Test and Infer.')
parser.add_argument('-l', '--lb',     type=float, help='lower bound', required=True)
parser.add_argument('-u', '--ub',     type=float, help='upper bound', required=True)
parser.add_argument('-p', '--prec',   type=float, help='precision',   required=True)
parser.add_argument('-r', '--radius', type=float, help='radius', required=True)
parser.add_argument('-v', '--verbose',            help='verbose', action='store_true', default=False)
parser.add_argument('-d', '--debug',              help='debug',   action='store_true', default=False)
parser.add_argument('srcfile', type=str, help='src file')
parser.add_argument('binfile', type=str, help='bin file')
args = parser.parse_args()
(lb, ub, prec, r, srcfile, binfile) = (args.lb, args.ub, args.prec, args.radius, args.srcfile, args.binfile)

# =================
# Logging
# =================
if args.verbose:
    logging.basicConfig(format='%(asctime)s --- %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p', level=logging.INFO)
elif args.debug:
    logging.basicConfig(format='%(asctime)s --- %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p', level=logging.DEBUG)
else:
    logging.basicConfig(format='%(asctime)s --- %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p')

# =================
# Check Arguments
# =================
tai='/Users/soonhok/work/tai/src/cil2smt2/main.native'
dreal='/Users/soonhok/bin/dReal'
for filename in [tai, dreal, srcfile, binfile]:
    check_file_or_die(filename)

# =================
# Start Main Loop
# =================
c = args.lb
d = min(c + r, args.ub)
eps = 10000.001
base = os.path.basename(binfile)

while c < ub:
    logging.info("Checking a range [%f, %f] with precision %f" % (c, d, prec))
    smt2_filename = get_filename([base, 'formula', c, d, prec], ext = 'smt2')
    smt2_result_filename = get_filename([base, 'dreal_output', c, d, prec])
    smt2_model_filename = get_filename([base, 'formula', c, d, prec], ext = 'smt2.model')

    smt2_file  = open(smt2_filename, 'w')
    smt2_result_file = open(smt2_result_filename, 'w')

    (test_out, v_p, v_o, info_filename) = test(binfile, math.sin, c, eps)
    print "INFO:", info_filename

    if test_out == False:
        logging.error("Test on %f is not passed." % c)
        exit(1)

    smt2_out = generate_SMT(tai, c, d, info_filename, srcfile, smt2_file)
    dreal_out = run_dReal(dreal, smt2_filename, prec, smt2_result_file)

    # TODO(soonhok): what if test_out is non-zero?

    if is_SAT(smt2_model_filename):
        # SAT Case
        logging.info("SAT: " + to_interval(c, d))
        model = read_model(smt2_model_filename)
        print_model(model)
        c_lb = model['x0'][0]
        c_ub = model['x0'][1]
        c_mid = c_lb / 2.0 + c_ub / 2.0
        test_out = test(binfile, math.sin, c, eps)
        if test_out == 0:
            # Test is passed. 'c_' is a spurious counterexample
            logging.info("Test on %f is passed. %f is a spurious counterexample." % (c_mid, c_mid))
            logging.info("Use smaller precision value: prec = %f." % prec)
            prec = prec / 2.0
        else:
            # Test is not passed. 'c_' was a real counterexample
            logging.info("Test on %f is not passed. %f is a real counterexample." % (c_mid, c_mid))
            d = c_lb
        # run the binary with x', check whether it's a false positive or not
        # if (false positive):
        #     run dReal with higher precision
        #     prec = prec / 2
        # if (true positive):
        #     reduce range [c, d] => [c, x']
        #     d = x'
        #     and check [c, x']
    else:
        # UNSAT Case
        logging.info("UNSAT: " + to_interval(c, d))
        c = d
        d = c + r
        prec = args.prec

    logging.info("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
