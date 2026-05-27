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


n80 = int(len(words) * 0.8)
n10 = int(len(words) * 0.1)
train_words = words[:n80]
val_words = words[n80 : n80 + n10]
test_words = words[n80 + n10 :]


def get_buckets(words):
    buckets = {}
    for w in words:
        if not buckets.get(len(w)):
            buckets[len(w)] = []
        buckets[len(w)].append(torch.tensor([stoi[c] for c in "." + w + "."], dtype=torch.long))
    for k in buckets.keys():
        buckets[k] = torch.stack(buckets[k])
    return buckets


train_buckets = get_buckets(train_words)
val_buckets = get_buckets(val_words)


C = 27
H = 512


class RNN(nn.Module):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.embed = nn.Embedding(num_embeddings=C, embedding_dim=H)
        self.W_state = nn.Linear(in_features=H, out_features=H)
        self.W_input = nn.Linear(in_features=H, out_features=H)
        self.W_output = nn.Linear(in_features=H, out_features=C)

    def forward(self, state, x):
        new_state = torch.tanh(self.W_state(state) + self.W_input(self.embed(x)))
        pred = self.W_output(new_state)
        return new_state, pred


@torch.no_grad()
def validation_loss():
    loss = 0
    total_steps = 0.0
    for b_len, b_ts in val_buckets.items():
        xs = b_ts.T
        state = torch.zeros((b_ts.shape[0], H))
        for i in range(b_len + 1):
            new_state, pred = model(state, xs[i])
            state = new_state
            loss += F.cross_entropy(pred, xs[i + 1])
            total_steps += 1
    return (loss / total_steps).item()


model = RNN()
adam = optim.Adam(model.parameters(), lr=0.001)
B = 32

for step in range(10000):
    bucket_len = len(train_words[torch.randint(0, len(train_words), (1,))[0]])
    bucket = train_buckets[bucket_len]
    batch = torch.randint(0, len(bucket), (B,))
    xs = bucket[batch].T
    loss = 0
    state = torch.zeros((B, H))
    for i in range(bucket_len + 1):
        new_state, pred = model(state, xs[i])
        state = new_state
        loss = loss + F.cross_entropy(pred, xs[i + 1])
    loss = loss / float(bucket_len + 1)
    adam.zero_grad()
    loss.backward()
    adam.step()
    if step % 777 == 0:
        print(validation_loss())

with torch.no_grad():
    for _ in range(100):
        x = "."
        state = torch.zeros((H,))
        l = 0
        while l < 27:
            new_state, pred = model(state, torch.tensor(stoi[x]))
            probs = F.softmax(pred, dim=-1)
            c = itos[probs.multinomial(num_samples=1).item()]
            if c == ".":
                break
            print(c, end="")
            l += 1
            state = new_state
            x = c
        print()
