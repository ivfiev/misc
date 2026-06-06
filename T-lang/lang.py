from model import *

D = 192
E = {e: [1.0 if j == i else 0.0 for j in range(D)] for i, e in enumerate("^abcdefghijklmnopqrstuvwxyz|?$")}
P = {p: [1.0 if j == p + len(E) else 0.0 for j in range(D)] for p in range(18)}


def un_E(u: vec) -> str:
    assert len(u) == len(E)
    assert sum(u) == 1.0
    for k, v in E.items():
        if v[: len(E)] == u:
            return k
    raise Exception("cannot unembed")


def build_ffn(codes: list[list]) -> FFN:
    logic = ["AND", "OR", "NOT"]
    other = ["ZERO"]
    n = max(1, sum(code[0] in [*logic, *other] for code in codes))
    input = [[0.0] * D for _ in range(n)]
    output = [[0.0] * n for _ in range(D)]
    bias_in = [0.0] * n
    bias_out = [0.0] * D
    for ip, code in enumerate(codes):
        c = code[0]
        if c in ["AND", "OR", "NOT"]:
            features = code[1]
            write = code[2]
            for f in features:
                input[ip][f] = 1.0
            bias_in[ip] = -len({*features}) + 1.0 if c == "AND" else 0.0
            for w in write:
                output[w][ip] = -3.0 if c == "NOT" else 1.0
                bias_out[w] = 1.0 if c == "NOT" else 0.0
        if c == "ZERO":
            bit = code[1]
            input[ip][bit] = 1.0
            output[bit][ip] = -1.0
    return FFN(t(input), bias_in, t(output), bias_out)


def build_attn(codes: list[list]) -> Attention:
    q, k, v, p = [], [], [], []
    for code in codes:
        c = code[0]
        features = code[1]
        m = {
            "QUERY": q,
            "KEY": k,
            "VALUE": v,
            "PROJ": p,
        }[c]
        for f in features:
            m.append([0] * D)
            m[-1][f] = 1.0
    return Attention(t(q), t(k), t(v), p)


def run(program: list[list], input: str):
    blocks = []
    unembed = None
    for code in program:
        if len(code) == 2:
            blocks.append(Block([build_attn(code) for code in code[0]], build_ffn(code[1])))
        elif len(code) == 1:
            unembed = code[0]
    return Transformer(blocks, E, P, unembed)(input)


def who(c):
    return next(i for i, e in enumerate(E[c]) if e == 1.0)


def where(n):
    return next(i for i, e in enumerate(P[n]) if e == 1.0)


def slice(base, count):
    return [x for x in range(base, base + count)]


# a = a - b
def sub_one_hot(a, b, c):
    return [
        *[["ZERO", i] for i in range(a, a + c)],
        *[["AND", [i, j], [a + (i - a) - (j - b)]] for j in range(b, b + c) for i in range(a, a + c) if (i - a) >= (j - b)],
    ]


# a = a + b
def add_one_hot(a, b, c):
    return [
        *[["ZERO", i] for i in range(a, a + c)],
        *[["AND", [i, j], [a + (i - a) + (j - b)]] for j in range(b, b + c) for i in range(a, a + c) if (i - a) + (j - b) < c],
    ]


# d = a[0:c] == b[0:c], assume only one 1
def eq_one_hot(a, b, c, d):
    return [["AND", [a + i, b + i], [d]] for i in range(c)]


# d = a[0:c] < b[0:c], assume only one 1
def lt_one_hot(a, b, c, d):
    return [["AND", [a + j, b + i], [d]] for i in range(c) for j in range(i)]


# d = a[0:c] > b[0:c], assume only one 1
def gt_one_hot(a, b, c, d):
    return [["AND", [a + i, b + j], [d]] for i in range(c) for j in range(i)]


class FeatureAllocator:
    def __init__(self):
        self.n = len(E) + len(P)
        self.EMB = 0
        self.POS = len(E)

    def alloc(self, range=1):
        assert self.n + range < D
        n = self.n
        self.n += range
        return n
