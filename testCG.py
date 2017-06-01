# Use python 3.x
import os
from subprocess import *
import sys
import test

test.runTest(excluded     = test.expectParseError + test.expectBindingError + test.expectTypeError,
             expectToFail = test.exceptCGError,
             outFile      = 'test_CG.log')
