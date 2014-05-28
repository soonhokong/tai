#!/usr/bin/env python
# Main Script for Test-and-Infer
# Author: Soonho Kong
#
import argparse
import os.path
import subprocess

tai='/Users/soonhok/work/tai/src/cil2smt2/main.native'
dreal='/Users/soonhok/bin/dReal'

def check_file_or_die(f):
    if (not os.path.isfile(f)):
        print "File " + f + " does not exist."
        exit(1)
def to_interval(lb, ub):
    return '[{0}, {1}]'.format(lb, ub)

parser = argparse.ArgumentParser(description='Test and Infer.')
parser.add_argument('-l', '--lb',     type=float, help='lower bound', required=True)
parser.add_argument('-u', '--ub',     type=float, help='upper bound', required=True)
parser.add_argument('-p', '--prec',   type=float, help='precision',   required=True)
parser.add_argument('-r', '--radius', type=float, help='radius', required=True)
parser.add_argument('srcfile', type=str, help='src file')
parser.add_argument('binfile', type=str, help='bin file')

args = parser.parse_args()

(lb, ub, prec, r, srcfile, binfile) = (args.lb, args.ub, args.prec, args.radius, args.srcfile, args.binfile)

for filename in [tai, dreal, srcfile, binfile]:
    check_file_or_die(filename)

c = args.lb
d = c + r

while c <= ub:
    print "[C, D] =", to_interval(c, d)

    def get_filename(s, ext=None, dir='tmp/'):
        ret = dir + '{0}_{1}_{2}_{3}'.format(s, str(c), str(d), str(prec))
        if ext:
            ret = ret + "." + ext
        return ret

    output_filename = get_filename('output')
    info_filename = get_filename('info')
    smt2_filename = get_filename('formula', ext = 'smt2')
    smt2_result_filename = get_filename('dreal_output')
    smt2_model_filename = get_filename('formula', ext = 'smt2.model')

    output_file = open(output_filename, 'w')
    info_file  = open(info_filename, 'w')
    smt2_file  = open(smt2_filename, 'w')
    smt2_result_file = open(smt2_result_filename, 'w')

    # Run Binary
    output = subprocess.check_call([binfile, str(lb)], stdout = output_file, stderr = info_file)

    # Run TAI
    output = subprocess.check_call([tai, "-l", str(lb), "-u", str(ub), "-i", info_filename, srcfile], stdout = smt2_file)

    # Run dReal
    output = subprocess.check_call([dreal, "--precision", str(prec), smt2_filename], stdout = smt2_result_file)

    if os.path.isfile(smt2_model_filename):
        # SAT Case
        print "SAT:", to_interval(lb, ub)
        # extract counterexample x' from the model
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
        print "UNSAT:", to_interval(lb, ub)
        c = d
        d = c + r

    print "output: ", output
