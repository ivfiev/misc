import torch
from torch import optim
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
        x = torch.tensor([stoi[c] for c in "." + w + "."], dtype=torch.long)
        y = torch.tensor([stoi[c] for c in "." + w[::-1] + "."], dtype=torch.long)
        buckets[len(w)].append((x, y))
    for k in buckets.keys():
        xs, ys = zip(*buckets[k])
        buckets[k] = (torch.stack(xs), torch.stack(ys))
    return buckets


train_buckets = get_buckets(train_words)
val_buckets = get_buckets(val_words)


C = 27
H = 128


class Encoder(nn.Module):
    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self.emb = nn.Embedding(num_embeddings=C, embedding_dim=H)
        self.W_h = nn.Linear(in_features=H, out_features=H)
        self.W_x = nn.Linear(in_features=H, out_features=H)

    def forward(self, h, x):
        return torch.tanh(self.W_h(h) + self.W_x(self.emb(x)))


class Decoder(nn.Module):
    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self.emb = nn.Embedding(num_embeddings=C, embedding_dim=H)
        self.W_h = nn.Linear(in_features=H, out_features=H)
        self.W_x = nn.Linear(in_features=H, out_features=H)
        self.W_a = nn.Linear(in_features=H, out_features=H, bias=False)
        self.W_y = nn.Linear(in_features=H, out_features=C)
        self.W_c = nn.Linear(in_features=H, out_features=H)

    def forward(self, hs, h, x):
        a = self.W_a(h).view(-1, 1, H) @ hs.transpose(-1, -2)
        a = F.softmax(a, dim=-1)
        a = (a @ hs).squeeze(1)
        h = torch.tanh(self.W_h(h) + self.W_x(self.emb(x)).squeeze() + self.W_c(a))
        y = self.W_y(h)
        return h, y


# @torch.no_grad()
# def validation_loss():
#     loss = 0
#     total_steps = 0.0
#     for b_len, b_ts in val_buckets.items():
#         xs = b_ts.T
#         state = torch.zeros((b_ts.shape[0], H))
#         for i in range(b_len + 1):
#             new_state, pred = model(state, xs[i])
#             state = new_state
#             loss += F.cross_entropy(pred, xs[i + 1])
#             total_steps += 1
#     return (loss / total_steps).item()


encoder = Encoder()
decoder = Decoder()
adam = optim.Adam([*encoder.parameters(), *decoder.parameters()], lr=0.0001)
B = 64

for step in range(10000):
    bucket_len = len(train_words[torch.randint(0, len(train_words), (1,))[0]])
    xs, ys = train_buckets[bucket_len]
    batch = torch.randint(0, len(xs), (B,))
    xs = xs[batch].T
    ys = ys[batch].T
    state = torch.zeros((B, H))
    hs = []

    for i in range(xs.shape[0]):
        state = encoder(state, xs[i])
        hs.append(state)

    loss = 0
    hs = torch.stack(hs).transpose(0, 1)
    state = hs[:, -1, :]
    x = torch.ones((B, 1), dtype=torch.long) * stoi["."]
    for i in range(ys.shape[0]):
        new_state, y = decoder(hs, state, x)
        state = new_state
        x = y.argmax(-1)
        # x = ys[i]
        loss = loss + F.cross_entropy(y, ys[i])

    loss = loss / float(ys.shape[0])
    adam.zero_grad()
    loss.backward()
    adam.step()
    if step % 100 == 0:
        print(loss.item())

with torch.no_grad():
    cases = [".tensor.", ".shape.", ".debugging."]
    for test in cases:
        x = torch.tensor([stoi[c] for c in test])
        state = torch.zeros(H)
        states = []
        for i in range(len(test)):
            state = encoder(state, x[i])
            states.append(state)
        states = torch.stack(states)
        x = stoi["."]
        for i in range(len(test)):
            h, y = decoder(states, state, torch.tensor(x))
            state = h
            x = y.argmax(-1)
            print(itos[x.item()], end="")
