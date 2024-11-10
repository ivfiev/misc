from functools import reduce


def lines(name):
    path = '../../build/release/' + name
    return open(path).read().split('\n')


def intersections(names):
    sets = []
    for name in names:
        sets.append(set(lines(name)))
    return reduce(lambda u, v: u.intersection(v), sets) - {''}


ts = intersections(['st1.txt', 'st2.txt', 'st3.txt', 'st4.txt'])
cts = intersections(['sc1.txt', 'sc2.txt', 'sc3.txt', 'sc4.txt'])

ts, cts = sorted(ts - cts), sorted(cts - ts)

print(len(ts))
print(len(cts))

for t in ts:
    tw = t.split(' ')
    for c in cts:
        cw = c.split(' ')
        if len(tw) == 4 and len(cw) == 4 and tw[3] == cw[3] and tw[0] == cw[0] and tw[1] != cw[1]:
            print('{' + t + '}\n{' + c + '}\n')
