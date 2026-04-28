from typing import Any
import torch
from torch import nn, optim
import torch.nn.functional as F
from torch.utils.data import DataLoader
from torchvision import datasets, transforms

device = "cuda"  # "cuda" if torch.cuda.is_available() else "cpu"

transform = transforms.ToTensor()

train_ds = datasets.CIFAR10(root="./data", train=True, download=True, transform=transform)
test_ds = datasets.CIFAR10(root="./data", train=False, download=True, transform=transform)

train_ld = DataLoader(train_ds, batch_size=64, shuffle=True)
test_ld = DataLoader(test_ds, batch_size=64)


class Model(nn.Module):
    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self.cnn = nn.Sequential(
            nn.Conv2d(3, 16, 5, padding=2),  # 3,32,32 -> 16,32,32
            nn.ReLU(),
            nn.MaxPool2d(2, 2),  # 16,32,32 -> 16,16,16
            nn.Conv2d(16, 32, 5, padding=2),  # 16,16,16 -> 32,16,16
            nn.ReLU(),
            nn.MaxPool2d(2, 2),  # 32,8,8
            nn.Conv2d(32, 64, 5, padding=2),  # 64,8,8
            nn.ReLU(),
            nn.Flatten(start_dim=1),
            nn.Linear(64 * 8 * 8, 128),
            nn.ReLU(),
            nn.Linear(128, 10),
        )

    def forward(self, x):
        return self.cnn(x)


model = Model().to(device)
adam = optim.Adam(model.parameters(), lr=0.0003)
print(sum(p.numel() for p in model.parameters() if p.requires_grad))
for epoch in range(5):
    for i, (x, y) in enumerate(train_ld):
        x, y = x.to(device), y.to(device)
        logits = model(x)
        loss = F.cross_entropy(logits, y)
        loss.backward()
        adam.step()
        adam.zero_grad()
        if i % 100 == 0:
            print(loss.item())

correct, total = 0, 0
with torch.no_grad():
    for x, y in test_ld:
        x, y = x.to(device), y.to(device)
        logits = model(x)
        predicted = logits.argmax(dim=-1)
        total += y.size(0)
        correct += (predicted == y).sum().item()
print(float(correct) / float(total))
