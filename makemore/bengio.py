import math
import torch
from torch import Tensor, optim
import string
import torch.nn.functional as F

torch.manual_seed(42)

words = open("names", "r").read().splitlines()
vocab = ["."] + list(string.ascii_lowercase)
stoi = {s: i for i, s in enumerate(vocab)}
itos = {i: s for s, i in stoi.items()}

T = 3
V = len(vocab)
C = 10


def compile(words):
    xs, ys = [], []
    for w in words:
        ctx = ["."] * T
        for c in w + ".":
            xs.append([stoi[c] for c in ctx[:T]])
            ys.append(stoi[c])
            ctx = ctx[1:] + [c]
    return torch.tensor(xs), torch.tensor(ys)


n80 = int(len(words) * 0.8)
n10 = int(len(words) * 0.1)
train_xs, train_ys = compile(words[:n80])
val_xs, val_ys = compile(words[n80 : n80 + n10])
test_xs, test_ys = compile(words[n80 + n10 :])

E = torch.randn((V, C))
W1 = torch.randn((T * C, 300)) / float(T * C) ** 0.5
B1 = torch.randn((300,)) * 0.01
W2 = torch.randn((300, V)) / 300**0.5
B2 = torch.randn((V,)) * 0.01
parameters: list[Tensor] = [E, W1, B1, W2, B2]


def forward(xs):
    return (E[xs].view(-1, T * C) @ W1 + B1).tanh() @ W2 + B2


print("#parameters", sum(p.nelement() for p in parameters))

for p in parameters:
    p.requires_grad = True

for lr, iters in [(0.1, 25000), (0.01, 10000)]:
    for _ in range(iters):
        batch = torch.randint(0, train_xs.shape[0], (64,))
        logits = forward(train_xs[batch])
        loss = F.cross_entropy(logits, train_ys[batch])
        for p in parameters:
            p.grad = None
        loss.backward()
        for p in parameters:
            p.data += -lr * p.grad  # pyright: ignore[reportOperatorIssue]

print("train", F.cross_entropy(forward(train_xs), train_ys).item())
print("val", F.cross_entropy(forward(val_xs), val_ys).item())
print("test", F.cross_entropy(forward(test_xs), test_ys).item())

with torch.no_grad():
    for _ in range(10):
        ctx = [".", ".", "."]
        while True:
            x = torch.tensor([stoi[c] for c in ctx], dtype=torch.int)
            logits = forward(x)
            probs = logits.softmax(dim=-1)
            sample = probs.multinomial(num_samples=1)
            c = itos[sample.item()]
            if c == ".":
                print()
                break
            else:
                print(c, end="")
            ctx = ctx[1:] + [c]
