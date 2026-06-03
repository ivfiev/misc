type vec = list[float]
type mat = list[list[float]]

D = 64
E = {e: [1.0 if j == i else 0 for j in range(D)] for i, e in enumerate("^abcde$")}
P = {p: [1.0 if j == p + len(E) else 0 for j in range(D)] for p in range(12)}


def mm(a: mat, b: mat) -> mat:
    assert all(len(r) == len(b) for r in a)
    return [[sum(a[r][i] * b[i][c] for i in range(len(a[0]))) for c in range(len(b[0]))] for r in range(len(a))]


def ma(a: mat, b: mat) -> mat:
    assert len(a) == len(b)
    return [[a[r][c] + b[r][c] for c in range(len(a[0]))] for r in range(len(a))]


def va(a: mat, u: vec) -> mat:
    assert len(a[0]) == len(u)
    return [[u[i] + a[r][i] for i in range(len(a[r]))] for r in range(len(a))]


def t(a: mat) -> mat:
    b = []
    for i in range(len(a[0])):
        b.append([])
        for j in range(len(a)):
            b[-1].append(a[j][i])
    return b


def binary_norm(a: mat) -> mat:
    return [[0.0 if x < 0.9 else 1.0 for x in r] for r in a]


def relu(a: mat) -> mat:
    return [[0.0 if x < 0.0 else x for x in r] for r in a]


def pm(a: mat):
    for u in a:
        print(u)
    print()


class Attention:
    def __init__(self, queries: mat, keys: mat, values: mat, proj: mat):
        self.queries = queries  # d x a
        self.keys = keys  # d x a
        self.values = values  # d x v
        self.proj = proj  # v x d

    def __call__(self, x) -> mat:
        q = mm(x, self.queries)  # n x a
        k = mm(x, self.keys)  # n x a
        v = mm(x, self.values)  # n x v
        qk = mm(q, t(k))  # n x n
        qk = self._hardmax(qk)
        pm(qk)
        x = mm(qk, v)  # n x v
        x = mm(x, self.proj)
        return x

    def _hardmax(self, qk: mat) -> mat:
        return [[1.0 if x == len(self.queries[0]) else 0.0 for x in r] for r in qk]


class FFN:
    def __init__(self, input: mat, bias_in: vec, output: mat, bias_out: vec):
        self.input = input
        self.bias_in = bias_in
        self.output = output
        self.bias_out = bias_out

    def __call__(self, x: mat) -> mat:
        x = mm(x, t(self.input))
        x = va(x, self.bias_in)
        x = relu(x)
        x = mm(x, t(self.output))
        x = va(x, self.bias_out)
        return x


class Block:
    def __init__(self, attn: list[Attention], ffn: FFN):
        self.attn = attn
        self.ffn = ffn

    def __call__(self, x: mat) -> mat:
        attns = [attn(x) for attn in self.attn]
        for a in attns:
            x = ma(x, a)
        x = binary_norm(x)
        x = ma(x, self.ffn(x))
        x = binary_norm(x)
        return x


class Transformer:
    def __init__(self, blocks: list[Block], unembed):
        self.blocks = blocks
        self.unembed = unembed

    def __call__(self, x: str):
        e = [E[c] for c in x]
        p = [P[i] for i in range(len(x))]
        m = ma(e, p)
        for b in self.blocks:
            m = b(m)
        return [[m[i][j] for j in self.unembed[1]] for i in self.unembed[0]]


def build_ffn(codes: list[list]) -> FFN:
    ip = 0
    n = max(1, sum(code[0] in ["AND", "OR", "NOT"] for code in codes))
    input = [[0.0] * D for _ in range(n)]
    output = [[0.0] * n for _ in range(D)]
    bias_in = [0.0] * n
    bias_out = [0.0] * D
    for code in codes:
        if code[0] in ["AND", "OR", "NOT"]:
            c = code[0]
            features = code[1]
            write = code[2]
            for f in features:
                input[ip][f] = 1.0 if c != "NOT" else -1.0
            bias_in[ip] = -len(features) + 1.0 if c == "AND" else 1.0 if c == "NOT" else 0.0
            for w in write:
                output[w][ip] = 1.0
            ip += 1
    return FFN(input, bias_in, output, bias_out)


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


def build_transformer(codes: list[list]):
    blocks = []
    unembed = None
    for code in codes:
        if len(code) == 2:
            blocks.append(Block([build_attn(code) for code in code[0]], build_ffn(code[1])))
        elif len(code) == 1:
            unembed = code[0]
    return Transformer(blocks, unembed)


def who(c):
    return next(i for i, e in enumerate(E[c]) if e == 1.0)


def where(n):
    return next(i for i, e in enumerate(P[n]) if e == 1.0)


def num(base, count):
    return [x for x in range(base, base + count + 1)]


if __name__ == "__main__":
    model = build_transformer(
        [
            [
                [
                    [
                        ["QUERY", [who("^")]],
                        ["KEY", [who("$")]],
                        ["VALUE", num(len(E), len(P))],
                        ["PROJ", num(30, len(P))],
                    ],
                ],
                [],
            ],
            [
                ([0], range(30, 43)),
            ],
        ]
    )
    pm(model("^aba$"))
