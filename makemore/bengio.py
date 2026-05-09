import math
import torch
from torch import Tensor, optim
import string
import torch.nn.functional as F

torch.manual_seed(2147483647)

words = open("names", "r").read().splitlines()
vocab = ["."] + list(string.ascii_lowercase)
stoi = {s: i for i, s in enumerate(vocab)}
itos = {i: s for s, i in stoi.items()}

T = 3
V = len(vocab)
C = 10
xs, ys = [], []

for w in words:
    ctx = ["."] * T
    for c in w + ".":
        xs.append([stoi[c] for c in ctx[:T]])
        ys.append(stoi[c])
        ctx = ctx[1:] + [c]

xs = torch.tensor(xs)
ys = torch.tensor(ys)

train_xs = xs[: int(xs.shape[0] * 0.8)]
train_ys = ys[: int(ys.shape[0] * 0.8)]
val_xs = xs[int(xs.shape[0] * 0.8) : int(xs.shape[0] * 0.9)]
val_ys = ys[int(ys.shape[0] * 0.8) : int(ys.shape[0] * 0.9)]
test_xs = xs[int(xs.shape[0] * 0.9) :]
test_ys = ys[int(ys.shape[0] * 0.9) :]

E = torch.randn((V, C), requires_grad=True)
W1 = torch.randn((T * C, 300), requires_grad=True)
B1 = torch.randn((300,), requires_grad=True)
W2 = torch.randn((300, V), requires_grad=True)
B2 = torch.randn((V,), requires_grad=True)


def forward(xs):
    return (E[xs].view(-1, T * C) @ W1 + B1).tanh() @ W2 + B2


parameters = [E, W1, B1, W2, B2]

print(sum(p.nelement() for p in parameters))

for lr, iters in [(0.1, 100000), (0.01, 100000)]:
    for _ in range(iters):
        batch = torch.randint(0, train_xs.shape[0], (64,))
        logits = forward(train_xs[batch])
        loss = F.cross_entropy(logits, train_ys[batch])
        for p in parameters:
            p.grad = None
        loss.backward()
        for p in parameters:
            p.data += -lr * p.grad

print(F.cross_entropy(forward(train_xs), train_ys).item())
print(F.cross_entropy(forward(val_xs), val_ys).item())
print(F.cross_entropy(forward(test_xs), test_ys).item())
