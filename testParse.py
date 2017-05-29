# Use python 3.x
import os
import subprocess
import sys

example_dir = 'examples_parsing'

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
        "======" + path + "======\n",
        prog
        ])

    print("Press a key")
    
    try:
        result = subprocess.check_output(['spl.exe', path]).decode('utf-8')
        outputs.extend(["AST:",result])
    except subprocess.CalledProcessError as e:
        print('Exception in ' + path + ':')
        print((e.output).decode('utf-8'))
        outputs.extend(["exception!\n", (e.output).decode('utf-8')])

with open('test_output_parsing.log', 'wb') as f:
    joinedOutputs = (os.linesep).join(outputs)
    f.write(joinedOutputs.encode())
