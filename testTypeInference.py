# Use python 3.x
import os
from subprocess import *
import sys

example_dir = 'examples_typing'

outputs = []

if len(sys.argv) > 1:
    testFiles = sys.argv[1:]
else:
    testFiles = [os.path.join(example_dir, file)
                 for file in os.listdir(example_dir)]

for path in testFiles:
    with open(path, 'r') as f:
        prog = f.read()

    outputs.extend([
        "======" + path + "======",
        prog
        ])

    proc = Popen(['spl.exe', path],
                         universal_newlines=True,
                         stdin=PIPE, stdout=PIPE, stderr=PIPE,
                         bufsize=0)
    try:
        outs, errs = proc.communicate("\n", timeout = 5)
    except TimeoutExpired:
        print("Timeout in " + path)
        outputs.append("Timeout!")
        proc.kill()
        outs, errs = proc.communicate()

    if(outs != ""):
        outputs.extend(["Program outputs:", outs])
    if(errs != ""):
        outputs.extend(["Program errors:", errs])    

with open('test_output_parsing.log', 'wb') as f:
    joinedOutputs = (os.linesep).join(outputs)
    f.write(joinedOutputs.encode())
