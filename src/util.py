#!/usr/bin/env python
"""
Utility Library
"""

__author__ = "Soonho Kong"
__copyright__ = "Copyright 2014, Soonho Kong"
__credits__ = ["Soonho Kong", "Sicun Gao", "Wei Chen", "Edmund Clarke"]
__license__ = "GPL"
__version__ = "0.1"
__maintainer__ = "Soonho Kong"
__email__ = "soonhok@cs.cmu.edu"
__status__ = "Production"

import logging
import os.path
import re
import subprocess

def run_test(binfile, c, output_file, info_file):
    logging.info("Test %s at %f" % (binfile, c))
    returncode = 0
    try:
        output = subprocess.check_call([binfile, str(c)], stdout = output_file, stderr = info_file)
    except subprocess.CalledProcessError as e:
        returncode = e.returncode
    logging.info("Test %s at %f, Result = %d" % (binfile, c, returncode))
    return returncode

def generate_SMT(tai, lb, ub, info_filename, srcfile, smt2_file):
    logging.info("Generate SMT2 for a range [%f, %f] info: %s, src: %s"
                               % (lb, ub, info_filename, srcfile))
    output = subprocess.check_call([tai, "-l", str(lb), "-u", str(ub), "-i", info_filename, srcfile], stdout = smt2_file)
    return output

def run_dReal(dreal, smt2_filename, prec, smt2_result_file):
    logging.info("Run dReal: %s" % smt2_filename)
    output = subprocess.check_call([dreal, "--precision", str(prec), "--model", smt2_filename], stdout = smt2_result_file)
    return output

def is_SAT(filename):
    return os.path.isfile(filename)

def check_file_or_die(f):
    if (not os.path.isfile(f)):
        logging.error("File " + f + " does not exist.")
        exit(1)

def to_interval(lb, ub):
    return '[{0}, {1}]'.format(lb, ub)

def read_model(filename):
    with open(filename) as f:
        content = f.readlines()
    model = {}
    for line in content:
        line = line.strip()
        m = re.search('(.+) : \[(.+), (.+)\];', line)
        var = m.group(1)
        lb = float.fromhex(m.group(2))
        ub = float.fromhex(m.group(3))
        model[var] = [lb, ub]
    return model

def print_model(model):
    logging.info("=============================== Model==============================")
    for key, value in model.iteritems():
        logging.info("\t %s = [%f, %f]" % (key, value[0], value[1]))
    logging.info("===================================================================")

def get_filename(s, lb, ub, prec, ext=None, dir='tmp/'):
    ret = dir + '{0}_{1}_{2}_{3}'.format(s, str(lb), str(ub), str(prec))
    if ext:
        ret = ret + "." + ext
    return ret
