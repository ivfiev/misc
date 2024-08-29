import os
import sys
from datetime import datetime

file = None


def init_file():
    global file
    os.makedirs('./py2py_logs', exist_ok=True)
    filename = datetime.now().strftime("./py2py_logs/logs_%Y-%m-%d-%H:%M:%S")
    file = open(filename, 'a')
    return file


def run():
    global file
    for line in sys.stdin:
        if file is None:
            file = init_file()
        if '<<END_OF_SESSION>>' in line:
            file.write(line)
            file.close()
            file = None
        else:
            file.write(line)


if __name__ == '__main__':
    try:
        run()
    except:
        if file:
            file.write('boo!')
            file.close()
