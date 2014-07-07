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
import mpmath

def get_filename(l, ext=None, dir='tmp/'):
    ret = dir + str.join("_", map(str, l))
    if ext:
        ret = ret + "." + ext
    return ret

def exec_file(filename, x):
    """
    Execute file ``filename`` with an argument ``x``.
    Return its output and created info file
    """
    base = os.path.basename(filename)
    info_filename = get_filename([base, "info", x])
    info_file  = open(info_filename, 'w')
    logging.info("Test %s at %f (save info at %s)" % (filename, x, info_filename))
    returncode = 0
    try:
        args = [filename, x]
        output_bytes = subprocess.check_output(map(str, args), stderr = info_file)
    except subprocess.CalledProcessError as e:
        returncode = e.returncode
    output = float.fromhex(output_bytes)

    logging.info("Test %s at %f, OUTPUT = %s" % (filename, x, output))
    logging.info("Test %s at %f, Result = %d" % (filename, x, returncode))
    info_file.close()
    return (output, info_filename)

def test(filename, oracle, x, eps):
    v_p, info = exec_file(filename, x)
    v_o = oracle(x)
    err = abs(v_o - v_p)
    logging.info("Test %s at %f" % (filename, x))
    logging.info("\t Oracle  output = %.15f" % v_o)
    logging.info("\t Program output = %.15f" % v_p)
    logging.info("\t Error          = %.15f" % err)
    logging.info("\t eps            = %.15f" % eps)
    if err > eps:
        return (False, v_p, v_o, info)
    else:
        return (True, v_p, v_o, info)

def run_test(binfile, c, info_file):
    logging.info("Test %s at %f" % (binfile, c))
    returncode = 0
    try:
        args = [binfile, c]
        output_bytes = subprocess.check_output(map(str, args), stderr = info_file)
    except subprocess.CalledProcessError as e:
        returncode = e.returncode
    output = float.fromhex(output_bytes)

    logging.info("Test %s at %f, OUTPUT = %s" % (binfile, c, output))
    logging.info("Test %s at %f, Result = %d" % (binfile, c, returncode))
    return returncode

def generate_SMT(tai, lb, ub, info_filename, srcfile, smt2_file):
    logging.info("Generate SMT2 for a range [%f, %f] info: %s, src: %s"
                               % (lb, ub, info_filename, srcfile))
    args = [tai,
            "-l", lb,
            "-u", ub,
            "-i", info_filename,
            srcfile]
    output = subprocess.check_call(map(str, args), stdout = smt2_file)
    return output

def run_dReal(dreal, smt2_filename, prec, smt2_result_file):
    logging.info("Run dReal: %s" % smt2_filename)
    args = [dreal,
            "--precision", prec,
            "--model",
            smt2_filename]
    try:
        output = subprocess.check_call(map(str, args), stdout = smt2_result_file)
    except subprocess.CalledProcessError as e:
        print str.join(" ", map(str, args))
        exit(1)
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
