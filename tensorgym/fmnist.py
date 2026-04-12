from typing import Any
import torch
from torch import Tensor, nn, optim
from torch.utils.data import DataLoader
from torchvision import datasets
from torchvision.transforms import ToTensor
import torch.nn.functional as F

device = "cpu"
training_data = datasets.FashionMNIST(root="data", train=True, download=True, transform=ToTensor())
test_data = datasets.FashionMNIST(root="data", train=False, download=True, transform=ToTensor())

B = 64

train_dataloader = DataLoader(training_data, batch_size=B)
test_dataloader = DataLoader(test_data, batch_size=B)


class FFNN(nn.Module):
    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self.flatten = nn.Flatten()
        self.layers = nn.Sequential(
            nn.Linear(28 * 28, 512),
            nn.ReLU(),
            nn.Linear(512, 512),
            nn.ReLU(),
            nn.Linear(512, 10),
        )

    def forward(self, x):
        x = self.flatten(x)
        logits = self.layers(x)
        return logits


model = FFNN().to(device)
opt = optim.Adam(model.parameters(), lr=0.001)


def train():
    model.train()
    for b, (X, y) in enumerate(train_dataloader):
        X, y = X.to(device), y.to(device)
        loss = F.cross_entropy(model(X), y)
        loss.backward()
        opt.step()
        opt.zero_grad()
        if b % 100 == 0:
            print(loss.item())


def test():
    model.eval()
    correct, total = 0, 0
    with torch.no_grad():
        for X, y in test_dataloader:
            X, y = X.to(device), y.to(device)
            prediction: Tensor = model(X)
            total += X.shape[0]
            correct += (prediction.argmax(dim=1) == y).type(torch.float).sum().item()
    print(f"accuracy: {float(correct)/float(total)}")


train()
test()
print(sum(p.numel() for p in model.parameters() if p.requires_grad))
