# Use python 3.x
import os
from subprocess import *
import sys
import test

test.runTest(excluded     = test.expectParseError + test.expectBindingError,
             expectToFail = test.expectTypeError,
             outFile      = 'test_Typeinference.log')
