# Use python 3.x
import os
import subprocess

example_dir = 'examples_typing'

outputs = []

for file in os.listdir(example_dir):
    path = os.path.join(example_dir, file)
    with open(path, 'r') as f:
        prog = f.read()

    outputs.extend([
        "======" + path + "======",
        prog
        ])

    print("Press a key")
    
    try:
        compiled = subprocess.check_output(['spl.exe', path]).decode('utf-8')
        outputs.append(compiled)
    except subprocess.CalledProcessError as e:
        print('Exception in ' + path + ':')
        print((e.output).decode('utf-8'))
        outputs.extend(["exception!", (e.output).decode('utf-8')])

with open('test_output_typing.log', 'w') as f:
    outputs = '\n'.join(outputs)
    f.write(outputs)
