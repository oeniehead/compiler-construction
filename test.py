# Use python 3.x
import os
from subprocess import *
import sys

TIMEOUT = -1
FAIL = 0
SUCCESS = 1

example_dir = 'examples'

expectParseError = [
    'empty.spl',
    'unbalanced_parenthesis.spl',
    'unbalanced_parenthesis2.spl',
    ]

expectBindingError = [
    '3D.spl',
    'booleans.spl',
    'fundecl.spl',
    'nothing.spl',
    'problematic.spl',
    'problematic_programs.spl',
    'stress.spl',
    'SumProduct.spl'
    ]

expectTypeError = [
    'arguments.spl',
    'constants.spl',
    'more_parenthesis.spl',
    'problematic_programs.spl',
    'unary_minus.spl',
	'recursion untyped 1.spl',
	'recursion untyped 2.spl',
	'simple recursion 2.spl',
    ]

exceptCGError = [
    ]


def test(testSpecs, outFile, timeout, logSucceededTests):
    succeededTests = []
    failedTests = []
    
    for (file,expRes) in testSpecs:
        testResult = lambda: None # dirty way to create a new object
        with open(file, 'r') as f:
            prog = f.read()

        proc = Popen(['spl.exe', file],
                             universal_newlines=True,
                             stdin=PIPE, stdout=PIPE, stderr=PIPE,
                             bufsize=0)
        try:
            outs, errs = proc.communicate("\n", timeout = timeout)
            testResult.succeeded = (
                expRes == SUCCESS and errs == ""
                or expRes == FAIL and errs != "")
        except TimeoutExpired:
            print("Timeout in " + file)
            testResult.succeeded = TIMEOUT
            proc.kill()
            outs, errs = proc.communicate()

        testResult.file = file
        testResult.prog = prog
        testResult.outs = outs
        testResult.errs = errs
        
        
        if(testResult.succeeded == SUCCESS):
            succeededTests.append(testResult)
        else:
            failedTests.append(testResult)

    print(str(len(succeededTests)) + " tests succeeded, " + str(len(failedTests)) + " tests failed")
    
    failedTests = sorted(failedTests, key=lambda t: t.succeeded, reverse=True)
    outputs = []

    outputs.append("============== failed tests ==============")
    for r in failedTests:
        outputs.extend([
            "======" + r.file + "======",
            r.prog
            ])
        if(r.succeeded == TIMEOUT): outputs.append("Timeout!")
        if(r.outs != ""): outputs.extend(["Program outputs:", r.outs])
        if(r.errs != ""): outputs.extend(["Program errors:", r.errs])

    if logSucceededTests:
        outputs.append("============== succeeded tests ==============")
        for r in succeededTests:
            outputs.extend([
                "======" + r.file + "======",
                r.prog
                ])
            if(r.outs != ""): outputs.extend(["Program outputs:", r.outs])
            if(r.errs != ""): outputs.extend(["Program errors:", r.errs])
    
    with open(outFile, 'wb') as f:
        joinedOutputs = (os.linesep).join(outputs)
        f.write(joinedOutputs.encode())

def runTest(excluded, expectToFail, outFile):
    logSucceededTests = sys.argv[2] in ['True', 'true', 't', '1'] if len(sys.argv) > 2 else True
    timeout           = int(sys.argv[1]) if len(sys.argv) > 1 else 5
    testSpecs = [(os.path.join(example_dir, file), FAIL if file in expectToFail else SUCCESS)
                 for file in os.listdir(example_dir) if file not in excluded and not file.endswith(".ssm")]
    test(testSpecs, outFile, timeout, logSucceededTests)
