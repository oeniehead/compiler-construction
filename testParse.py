# Use python 3.x
import os
from subprocess import *
import sys
import test

test.runTest(excluded     = [],
             expectToFail = test.expectParseError,
             outFile      = 'test_parse.log')
