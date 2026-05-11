import math
import torch
from torch import Tensor, optim
import string
import torch.nn.functional as F
from torch import nn

torch.manual_seed(42)

words = open("names", "r").read().splitlines()
vocab = ["."] + list(string.ascii_lowercase)
stoi = {s: i for i, s in enumerate(vocab)}
itos = {i: s for s, i in stoi.items()}


def compile(words):
    xs, ys = [], []
    for w in words:
        ctx = ["."] * 8
        for c in w + ".":
            xs.append([stoi[c] for c in ctx[:8]])
            ys.append(stoi[c])
            ctx = ctx[1:] + [c]
    return torch.tensor(xs), torch.tensor(ys)


n80 = int(len(words) * 0.8)
n10 = int(len(words) * 0.1)
train_xs, train_ys = compile(words[:n80])
val_xs, val_ys = compile(words[n80 : n80 + n10])
test_xs, test_ys = compile(words[n80 + n10 :])


class FlattenFew(nn.Module):
    def __init__(self, n):
        super().__init__()
        self.n = n

    def __call__(self, x):
        if x.ndim == 3:
            b, n, c = x.shape
            return x.view(b, n // self.n, c * self.n)
        if x.ndim == 2:
            n, c = x.shape
            return x.view(n // self.n, c * self.n)


C = 32
H = 128
model = nn.Sequential(
    nn.Embedding(num_embeddings=27, embedding_dim=C),
    FlattenFew(2),
    nn.Linear(2 * C, H),
    nn.LayerNorm(H),
    nn.Tanh(),
    FlattenFew(2),
    nn.Linear(2 * H, H),
    nn.LayerNorm(H),
    nn.Tanh(),
    FlattenFew(2),
    nn.Linear(2 * H, H),
    nn.LayerNorm(H),
    nn.Tanh(),
    nn.Linear(H, 27),
    nn.Flatten(start_dim=1, end_dim=2),
)

print("#parameters", sum(p.nelement() for p in model.parameters()))

adam = optim.Adam(model.parameters(), lr=0.0002)

for i in range(100000):
    batch = torch.randint(0, train_xs.shape[0], (64,))
    logits = model(train_xs[batch])
    loss = F.cross_entropy(logits, train_ys[batch])
    adam.zero_grad()
    loss.backward()
    adam.step()
    if i == 70000:
        print("lr decay")
        for param_group in adam.param_groups:
            param_group["lr"] = 0.00001
    if i % 1000 == 0:
        print(F.cross_entropy(model(val_xs), val_ys).item())

print("train", F.cross_entropy(model(train_xs), train_ys).item())
print("val", F.cross_entropy(model(val_xs), val_ys).item())
print("test", F.cross_entropy(model(test_xs), test_ys).item())

with torch.no_grad():
    for _ in range(10):
        ctx = ["."] * 8
        while True:
            x = torch.tensor([stoi[c] for c in ctx], dtype=torch.int).view(1, -1)
            logits = model(x)
            probs = logits.softmax(dim=-1)
            sample = probs.multinomial(num_samples=1)
            c = itos[sample.item()]
            if c == ".":
                print()
                break
            else:
                print(c, end="")
            ctx = ctx[1:] + [c]
