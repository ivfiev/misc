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

N = torch.zeros((len(vocab), len(vocab)), dtype=torch.int32)

bigrams = {}
for w in words:
    w = ["."] + list(w) + ["."]
    for b in zip(w, w[1:]):
        N[stoi[b[0]], stoi[b[1]]] += 1

P = (N + 1).float() / N.sum(dim=-1, keepdim=True)
for _ in range(5):
    ix = 0
    while True:
        ix = int(torch.multinomial(P[ix], num_samples=1, replacement=True).item())
        if ix == 0:
            print()
            break
        print(itos[ix], end="")

nll = 0.0
count = 0
for w in words:
    w = ["."] + list(w) + ["."]
    for b in zip(w, w[1:]):
        nll += math.log(P[stoi[b[0]], stoi[b[1]]])
        count += 1
nll = -nll / float(count)
print("nll", nll)

xs, ys = [], []
for w in words:
    w = ["."] + list(w) + ["."]
    for b in zip(w, w[1:]):
        xs.append(stoi[b[0]])
        ys.append(stoi[b[1]])
xs = torch.tensor(xs, dtype=torch.int64)
ys = torch.tensor(ys, dtype=torch.int64)

x: Tensor = F.one_hot(xs, num_classes=len(vocab)).float()
W = torch.randn((len(vocab), len(vocab)), requires_grad=True)

for _ in range(25):
    log_counts = x @ W
    probs = F.softmax(log_counts, dim=-1)
    log_likelihood = probs[torch.arange(0, x.shape[0]), ys].log()
    log_likelihood_correct_token = log_likelihood.mean()
    loss = -log_likelihood_correct_token
    W.grad = None
    loss.backward()
    W.data += -50 * W.grad
    print(loss.item())
