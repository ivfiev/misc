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


class LSTM(nn.Module):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.embed = nn.Embedding(num_embeddings=C, embedding_dim=H)
        self.W_i = nn.Linear(in_features=2 * H, out_features=H)
        self.W_f = nn.Linear(in_features=2 * H, out_features=H)
        self.W_o = nn.Linear(in_features=2 * H, out_features=H)
        self.W_c = nn.Linear(in_features=2 * H, out_features=H)
        self.W_p = nn.Linear(in_features=H, out_features=C)

    def forward(self, c, h, x):
        x = self.embed(x)
        hx = torch.cat((h, x), dim=-1)
        f = torch.sigmoid(self.W_f(hx))
        i = torch.sigmoid(self.W_i(hx))
        o = torch.sigmoid(self.W_o(hx))
        c_ = torch.tanh(self.W_c(hx))
        c = f * c + i * c_
        h = o * torch.tanh(c)
        p = self.W_p(h)
        return c, h, p


@torch.no_grad()
def validation_loss():
    loss = 0
    total_steps = 0.0
    for b_len, b_ts in val_buckets.items():
        xs = b_ts.T
        c = torch.zeros((b_ts.shape[0], H))
        h = torch.zeros((b_ts.shape[0], H))
        for i in range(b_len + 1):
            c, h, p = model(c, h, xs[i])
            loss += F.cross_entropy(p, xs[i + 1])
            total_steps += 1
    return (loss / total_steps).item()


model = LSTM()
adam = optim.Adam(model.parameters(), lr=0.001)
B = 32

for step in range(10000):
    bucket_len = len(train_words[torch.randint(0, len(train_words), (1,))[0]])
    bucket = train_buckets[bucket_len]
    batch = torch.randint(0, len(bucket), (B,))
    xs = bucket[batch].T
    loss = 0
    c = torch.zeros((B, H))
    h = torch.zeros((B, H))
    for i in range(bucket_len + 1):
        c, h, p = model(c, h, xs[i])
        loss = loss + F.cross_entropy(p, xs[i + 1])
    loss = loss / float(bucket_len + 1)
    adam.zero_grad()
    loss.backward()
    adam.step()
    if step % 777 == 0:
        print(validation_loss())

with torch.no_grad():
    for _ in range(100):
        x = "."
        c = torch.zeros((H,))
        h = torch.zeros((H,))
        l = 0
        while l < 27:
            c, h, p = model(c, h, torch.tensor(stoi[x]))
            probs = F.softmax(p, dim=-1)
            s = itos[probs.multinomial(num_samples=1).item()]
            if s == ".":
                break
            print(s, end="")
            l += 1
            x = s
        print()
