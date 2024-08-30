import json
import os
import sys
import time


def read_lines():
    lines = []
    while True:
        str = sys.stdin.readline()
        if str:
            lines.append(str.rstrip())
        else:
            return lines


def get_val(line, key):
    i = line.find(key)
    if i > 0:
        return line[i + len(key):line.find(']', i)]
    return None


def run():
    times = dict()
    prev = None
    os.set_blocking(sys.stdin.fileno(), False)
    while True:
        model = {'graph': dict(), 'dead': []}
        changed = False
        lines = read_lines()
        for line in lines:
            if '<END_OF_SESSION>' in line:
                times = dict()
            if '[conn:' in line:
                node = get_val(line, 'node:')
                nodes = get_val(line, 'conn:').split(',')
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
