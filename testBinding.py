# Use python 3.x
import os
from subprocess import *
import sys
import test

test.runTest(excluded     = test.expectParseError,
             expectToFail = test.expectBindingError,
             outFile      = 'test_binding.log')
