import math
from typing import Any
import torch
from torch import Tensor, nn, optim
import torch.nn.functional as F

torch.manual_seed(42)

text = open("./data/names.txt").read()
vocab = sorted(list(set(text)))
latvian = "\n".join(f"{name}s" for name in text.split())
stoi = {ch: i for i, ch in enumerate(vocab)}
itos = {i: ch for i, ch in enumerate(vocab)}
encode = lambda s: [stoi[c] for c in s]
decode = lambda i: "".join([itos[i] for i in i])

data = torch.tensor(encode(text), dtype=torch.long)
datas = torch.tensor(encode(latvian), dtype=torch.long)
split = int(0.9 * len(data))
train_data = data[:split]
val_data = data[split:]

B = 16
T = 8
C = 16
V = len(vocab)
A = 2
H = C // A
M = 2
L = 4  # LoRA rank

device = "cuda" if torch.cuda.is_available() else "cpu"
tmask = (torch.tril(torch.ones(T, T)) == 0).to(device)


def get_batch(data: Tensor) -> tuple[Tensor, Tensor]:
    ixs = torch.randint(len(data) - T, (B,))
    x = torch.stack([data[i : i + T] for i in ixs])
    y = torch.stack([data[i + 1 : i + T + 1] for i in ixs])
    return x.to(device), y.to(device)


class LoraLinear(nn.Module):
    def __init__(self, in_features: int, out_features: int, rank: int, bias=True, alpha=1.0) -> None:
        super().__init__()
        self.base = nn.Linear(in_features, out_features, bias=bias)
        self.loraA = nn.Parameter(torch.randn((in_features, rank)) * 0.025)
        self.loraB = nn.Parameter(torch.zeros((rank, out_features)))
        self.scale = alpha / float(rank)
        self.loraA.requires_grad = False
        self.loraB.requires_grad = False

    def forward(self, x):
        base = self.base(x)
        if self.enabled():
            lora = ((x @ self.loraA) @ self.loraB) * self.scale
            return base + lora
        return base

    def enabled(self):
        return self.loraA.requires_grad and self.loraB.requires_grad


class Block(nn.Module):
    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self.norm_attn = nn.LayerNorm(C)
        self.qkv = LoraLinear(C, 3 * C, bias=False, rank=L)
        self.proj = nn.Linear(C, C, bias=False)
        self.norm_mlp = nn.LayerNorm(C)
        self.mlp = nn.Sequential(
            LoraLinear(C, M * C, rank=L),
            nn.GELU(),
            LoraLinear(M * C, C, rank=L),
        )

    def forward(self, btc):
        b, t, _ = btc.shape
        x: Tensor = self.norm_attn(btc)
        qkv: Tensor = self.qkv(x)
        q, k, v = qkv.chunk(3, dim=-1)
        q = q.view(b, t, A, H).transpose(1, 2)
        k = k.view(b, t, A, H).transpose(1, 2)
        v = v.view(b, t, A, H).transpose(1, 2)
        qk = (q @ k.transpose(-2, -1)) / math.sqrt(H)
        qk = qk.masked_fill(tmask[:t, :t], float("-inf"))
        x = F.softmax(qk, dim=-1) @ v
        x = x.transpose(1, 2).contiguous().view(b, t, C)
        x = self.proj(x)
        btc = btc + x
        x = self.norm_mlp(btc)
        x = self.mlp(x)
        btc = btc + x
        return btc


class Transformer(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        self.embeddings = nn.Embedding(V, C)
        self.positions = nn.Embedding(T, C)
        self.blocks = nn.Sequential(*[Block() for _ in range(2)])
        self.norm = nn.LayerNorm(C)
        self.unembed = LoraLinear(C, V, rank=L)
        self.toggle_lora(False)

    def forward(self, bt, ys=None):
        b, t = bt.shape
        btc = self.embeddings(bt) + self.positions(torch.arange(t, device=device))
        btc = self.blocks(btc)
        btc = self.norm(btc)
        logits = self.unembed(btc)
        if ys is None:
            loss = None
        else:
            logits = logits.view(B * T, V)
            ys = ys.view(B * T)
            loss = F.cross_entropy(logits, ys)
        return logits, loss

    def generate(self, prompt: str, n: int) -> str:
        gen = prompt
        for _ in range(n):
            x = torch.tensor(encode(gen[max(0, len(gen) - T) :])).view(1, -1)
            logits, _ = self(x)
            probs = F.softmax(logits, dim=-1)
            token = probs[0, -1, :].multinomial(1).tolist()
            gen = gen + decode(token)
        return gen

    def toggle_lora(self, flag: bool):
        for name, p in self.named_parameters():
            if "lora" in name:
                p.requires_grad = flag
            else:
                p.requires_grad = not flag


model = Transformer().to(device)
print(sum(p.numel() for p in model.parameters() if p.requires_grad))
adam = optim.Adam(model.parameters(), lr=0.01)
for i in range(1000):
    x, y = get_batch(data)
    logits, loss = model(x, y)
    loss.backward()
    adam.step()
    adam.zero_grad()
    if i % 100 == 0:
        print(loss.item())


model.toggle_lora(True)
print(sum(p.numel() for p in model.parameters() if p.requires_grad))
adam = optim.Adam(model.parameters(), lr=0.01)
for i in range(100):
    x, y = get_batch(datas)
    logits, loss = model(x, y)
    loss.backward()
    adam.step()
    adam.zero_grad()
    if i % 100 == 0:
        print(loss.item())

with torch.no_grad():
    print("\nBase model output:")
    model.toggle_lora(False)
    print(model.generate("zzzz", 100))
    print("\nLoRA output:")
    model.toggle_lora(True)
    print(model.generate("zzzz", 100))

# keepdim=True
