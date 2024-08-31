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
    graph = dict()
    times = dict()
    prev = None
    os.set_blocking(sys.stdin.fileno(), False)
    while True:
        errors = []
        changed = False
        lines = read_lines()
        for line in lines:
            if '<END_OF_SESSION>' in line:
                times = dict()
            elif 'ERROR' in line:
                errors.append(line)
            elif '[conn:' in line:
                node = get_val(line, 'node:')
                nodes = get_val(line, 'conn:').split(',')
                known = get_val(line, 'total:').split(',')
                for n in known:
                    if not graph.get(n):
                        graph[n] = []
                graph[node] = [] if '' in nodes else sorted(nodes)
                times[node] = time.time()
                changed = True
        graph = dict(sorted(graph.items()))
        dead = [n for n, t in times.items() if time.time() - t > 10]
        model = {'graph': graph, 'dead': dead, 'errors': errors}
        new = json.dumps(model)
        if changed and new != prev:
            print(new, flush=True)
            prev = new
        time.sleep(0.1)


if __name__ == '__main__':
    run()
