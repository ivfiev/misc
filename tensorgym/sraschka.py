from typing import Any
import torch
from torch import Tensor, nn
from torch.utils.data import DataLoader, Dataset
import torch.nn.functional as F

torch.manual_seed(42)
device = "cpu"


class Model(nn.Module):
    def __init__(self, inputs: int, outputs: int, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self.layers = nn.Sequential(
            nn.Linear(inputs, 30),
            nn.ReLU(),
            nn.Linear(30, 20),
            nn.ReLU(),
            nn.Linear(20, 10),
            nn.ReLU(),
            nn.Linear(10, outputs),
        )

    def forward(self, x):
        logits = self.layers(x)
        return logits


model = Model(2, 2).to(device)

xs = torch.tensor(
    [
        [0.1, 1],
        [11, -1],
        [-2, -1],
        [-0.3, -7],
        [-5, 3],
        [3, 0.1],
    ]
)
y = torch.tensor([1, 1, 0, 0, 0, 1])


class XsDataset(Dataset):
    def __init__(self, xs: Tensor, y: Tensor) -> None:
        super().__init__()
        self.xs = xs
        self.y = y

    def __getitem__(self, i):
        return self.xs[i], self.y[i]

    def __len__(self):
        return self.xs.shape[0]


data = XsDataset(xs, y)
loader = DataLoader(
    dataset=data,
    batch_size=2,
    shuffle=True,
    num_workers=0,
    drop_last=True,
)

adam = torch.optim.Adam(model.parameters(), lr=0.1)
epochs = 3

for e in range(epochs):

    model.train()
    for b, (x, y) in enumerate(loader):
        logits = model(x)
        loss = F.cross_entropy(logits, y)
        adam.zero_grad()
        loss.backward()
        adam.step()
        print(float(loss.detach()))

    model.eval()
    ...


def predict(xs: Tensor) -> list[int]:
    logits = model(xs)
    out = torch.argmax(logits, dim=-1)
    return out.tolist()


test = torch.tensor(
    [
        [-1.0, 1.0],
        [-4.0, -2.0],
        [-0.3, 0.4],
        [1.0, 5.0],
        [0.3, -0.2],
        [3, 0.8],
    ]
)
print(predict(test))
