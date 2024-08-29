import json
import os
import sys
import time

times = dict()
prev = None
model = {'graph': dict(), 'dead': []}


def read_lines():
    lines = []
    while True:
        str = sys.stdin.readline()
        if str:
            lines.append(str.rstrip())
        else:
            return lines


def run():
    global prev
    os.set_blocking(sys.stdin.fileno(), False)
    while True:
        changed = False
        lines = read_lines()
        for (i, j) in zip(lines, lines[1:]):
            if j.startswith('conn'):
                node = i.split(' ')[1]
                nodes = j.split(' ')[-1].split(',')
                model['graph'][node] = [] if '' in nodes else sorted(nodes)
                times[node] = time.time()
                changed = True
        model['graph'] = dict(sorted(model['graph'].items()))
        model['dead'] = [n for n, t in times.items() if time.time() - t > 10]
        new = json.dumps(model)
        if changed and new != prev:
            print(new, flush=True)
            prev = new
        time.sleep(0.1)


if __name__ == '__main__':
    run()
