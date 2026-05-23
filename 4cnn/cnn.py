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

CLASSES = len(base_dataset.classes)
LABELS = base_dataset.classes
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


def cnn1():
    return nn.Sequential(
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
        nn.Linear(64, CLASSES),
    ).to(device=device)


class ConvBlock(nn.Module):
    def __init__(self, i, o, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.block = nn.Sequential(
            nn.Conv2d(in_channels=i, out_channels=o, kernel_size=3, stride=1, padding=1),
            nn.BatchNorm2d(o),
            nn.ReLU(),
            nn.Conv2d(in_channels=o, out_channels=o, kernel_size=3, stride=1, padding=1),
            nn.BatchNorm2d(o),
        )
        self.reshape = nn.Identity() if i == o else nn.Conv2d(in_channels=i, out_channels=o, kernel_size=1, stride=1, padding=0)

    def forward(self, x):
        return F.relu(self.reshape(x) + self.block(x))


def cnn2():
    return nn.Sequential(
        ConvBlock(3, 32),
        ConvBlock(32, 32),
        nn.MaxPool2d(2, 2),
        ConvBlock(32, 64),
        ConvBlock(64, 64),
        nn.MaxPool2d(2, 2),
        ConvBlock(64, 128),
        ConvBlock(128, 128),
        nn.MaxPool2d(2, 2),
        ConvBlock(128, 192),
        ConvBlock(192, 192),
        nn.MaxPool2d(2, 2),
        ConvBlock(192, 256),
        ConvBlock(256, 256),
        nn.AdaptiveAvgPool2d((1, 1)),
        nn.Flatten(),
        nn.Linear(1 * 1 * 256, CLASSES),
    ).to(device=device)


cnn = cnn2()
adam = optim.Adam(cnn.parameters(), lr=0.0003)

print("params: ", sum(p.numel() for p in cnn.parameters() if p.requires_grad))
print("random: ", -math.log(1.0 / float(CLASSES)))


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


@torch.no_grad()
def confusion():
    cnn.eval()
    m = torch.zeros((CLASSES, CLASSES))
    for x, y in val_loader:
        x, y = x.to(device), y.to(device)
        logits = cnn(x)
        predicted = logits.argmax(dim=-1)
        for y, p in zip(y, predicted):
            m[y, p] += 1
    m = m / (m.sum(dim=-1, keepdim=True) + 1e-8)
    print("     " + "".join(l.ljust(6) for l in LABELS))
    for i in range(CLASSES):
        print(LABELS[i].rjust(4), end=" ")
        for j in range(CLASSES):
            print(f"{m[i,j].item():.3f}", end=" ")
        print()


def set_lr(e: int):
    def apply(lr):
        for param_group in adam.param_groups:
            param_group["lr"] = lr

    if 7 <= e:
        apply(0.00005)


for e in range(15):
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
    print(f"epoch {1+e} avg train loss: {epoch_loss / len(train_loader):.3f}, val accuracy: {accuracy():.3f}")

confusion()
