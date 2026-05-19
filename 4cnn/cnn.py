import math
import torch
from torchvision import datasets, transforms
from torch.utils.data import DataLoader, Subset, random_split
from torch import nn, optim
import torch.nn.functional as F

device = "cuda"
IMG_SIZE = 128

aug_dataset = datasets.ImageFolder(
    root="images",
    transform=transforms.Compose(
        [
            transforms.Resize((IMG_SIZE, IMG_SIZE)),
            transforms.RandomHorizontalFlip(),
            transforms.RandomRotation(15),
            transforms.ToTensor(),
            transforms.Normalize(
                mean=[0.5] * 3,
                std=[0.5] * 3,
            ),
        ]
    ),
)

base_dataset = datasets.ImageFolder(
    root="images",
    transform=transforms.Compose(
        [
            transforms.Resize((IMG_SIZE, IMG_SIZE)),
            transforms.ToTensor(),
            transforms.Normalize(
                mean=[0.5] * 3,
                std=[0.5] * 3,
            ),
        ]
    ),
)

indices = torch.randperm(len(base_dataset))
train_size = int(0.8 * len(base_dataset))
val_size = int(0.1 * len(base_dataset))
test_size = len(base_dataset) - train_size - val_size

train_set = Subset(dataset=aug_dataset, indices=indices[:train_size])
val_set = Subset(dataset=base_dataset, indices=indices[train_size : train_size + val_size])
test_set = Subset(dataset=base_dataset, indices=indices[train_size + val_size :])

train_loader = DataLoader(train_set, batch_size=32, shuffle=True)
val_loader = DataLoader(val_set, batch_size=32)
test_loader = DataLoader(test_set, batch_size=32)

print(len(train_set), len(val_set), len(test_set))
print(len(train_loader), len(val_loader), len(test_loader))

cnn = nn.Sequential(
    nn.Conv2d(3, 32, 3, padding=1, stride=1),  # 128
    nn.BatchNorm2d(32),
    nn.ReLU(),
    nn.Conv2d(32, 64, 3, padding=1, stride=1),  # 128
    nn.BatchNorm2d(64),
    nn.ReLU(),
    nn.MaxPool2d(2, 2),  # 64
    nn.Conv2d(64, 96, 3, padding=1, stride=1),
    nn.BatchNorm2d(96),
    nn.ReLU(),
    nn.MaxPool2d(2, 2),  # 32
    nn.Conv2d(96, 128, 3, padding=1, stride=1),
    nn.BatchNorm2d(128),
    nn.ReLU(),
    nn.MaxPool2d(2, 2),  # 16
    nn.Conv2d(128, 192, 3, padding=1, stride=1),
    nn.BatchNorm2d(192),
    nn.ReLU(),
    nn.MaxPool2d(2, 2),  # 8
    nn.Conv2d(192, 256, 3, padding=1, stride=1),
    nn.BatchNorm2d(256),
    nn.ReLU(),
    nn.AdaptiveAvgPool2d((1, 1)),  # 1
    nn.Flatten(start_dim=1),
    nn.Linear(256 * 1 * 1, 64),
    nn.BatchNorm1d(64),
    nn.ReLU(),
    nn.Linear(64, 2),
).to(device=device)


adam = optim.Adam(cnn.parameters(), lr=0.0003)

print("params: ", sum(p.numel() for p in cnn.parameters() if p.requires_grad))
print("random: ", -math.log(0.5))


@torch.no_grad()
def accuracy():
    correct, total = 0, 0
    for x, y in val_loader:
        x, y = x.to(device), y.to(device)
        logits = cnn(x)
        predicted = logits.argmax(dim=-1)
        total += y.size(0)
        correct += (predicted == y).sum().item()
    return float(correct) / float(total)


def set_lr(e: int):
    def apply(lr):
        for param_group in adam.param_groups:
            param_group["lr"] = lr

    if 1 <= e <= 2:
        apply(0.0002)
    elif 2 <= e <= 7:
        apply(0.00005)
    elif 7 <= e:
        apply(0.00001)


for e in range(10):
    epoch_loss = 0.0
    set_lr(1 + e)
    cnn.train()
    for i, (x, y) in enumerate(train_loader):
        x, y = x.to(device), y.to(device)
        logits = cnn(x)
        loss = F.cross_entropy(logits, y)
        adam.zero_grad()
        loss.backward()
        adam.step()
        epoch_loss += loss.item()
    cnn.eval()
    print(f"epoch {1+e} avg train loss: {epoch_loss / len(train_loader):.3}, val accuracy: {accuracy():.3}")
