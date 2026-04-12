import math
from typing import Any
from torch import Tensor, nn
import torch
import torch.nn.functional as F


def broadcast(x: Tensor, u: int) -> Tensor:
    return x - x + u


# print(broadcast(torch.tensor([[1.0, 2.0], [2.0, 3.0], [3.0, 4.0]]), 14))


def drop_most_0s(x: Tensor) -> Tensor:
    counts = torch.count_nonzero(x, dim=0)
    col = torch.argmin(counts)
    return torch.cat((x[:, :col], x[:, 1 + col :]), dim=1)


def drop_most_0s2(x: Tensor) -> Tensor:
    counts = torch.count_nonzero(x, dim=0)
    col = torch.argmin(counts)
    # mask = [i != col for i in range(x.shape[1])]
    ixs = [i for i in range(x.shape[1]) if i != col]
    return x[:, ixs]


# print(drop_most_0s(torch.tensor([[1.0, 2.0, 1.0, 3.0], [2.0, 3.0, 1.0, 4.0], [3.0, 0.0, 1.0, 5.0]])))
# print(drop_most_0s2(torch.tensor([[1.0, 2.0, 1.0, 3.0], [2.0, 3.0, 1.0, 4.0], [3.0, 0.0, 1.0, 5.0]])))


def top_student(x: Tensor, y: Tensor) -> int:
    return int(torch.argmax(torch.sum(x * y, dim=1)))


# print(top_student(torch.tensor([[1, 0, 1], [0, 1, 1], [1, 1, 0]]), torch.tensor([2, 3, 1])))
# print(top_student(torch.tensor([[1, 1], [0, 1], [1, 0]]), torch.tensor([2, 3])))


def flatten(x: Tensor) -> Tensor:
    size = torch.numel(x)
    new_shape = [size] + [-1] * (len(x.shape) - 1)
    # return x.view(*new_shape)
    return x.view(-1)


# print(flatten(torch.tensor([[1, 1], [0, 1], [1, 0]])))


def variance(x: Tensor) -> float:
    u = torch.mean(x)
    v = torch.mean(torch.pow(x - u, 2))
    return float(v)


# print(variance(torch.tensor([1.0, 2.0, 3.0, 4.0, 5.0])))


def softmax(x: Tensor) -> Tensor:
    x = x - torch.max(x, dim=1).values.view(-1, 1)
    x = x.exp()
    s = x.sum(dim=1)
    # x.numel()
    # print(s.shape)
    # print(x.shape)
    return x / s.view(-1, 1)  # [X,Y,Z] + [U] = [X,Y,Z] + [1,1,U]. -1: can infer dims from size


# print(softmax(torch.tensor([[1.0, 2.0, 3.0], [2.0, 4.0, 6.0]])))


class SimpleMLP(nn.Module):
    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self.layers = nn.Sequential(
            nn.Linear(8, 4),
            nn.ReLU(),
            nn.Linear(4, 4),
            nn.ReLU(),
            nn.Linear(4, 2),
        )

    def forward(self, x):
        return self.layers(x)


# torch.manual_seed(0)
# mlp = SimpleMLP()
# print(mlp(torch.tensor([[0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8], [0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1]])))


def attention(query: torch.Tensor, key: torch.Tensor, value: torch.Tensor) -> Tensor:
    qk = query @ key.T
    qk /= math.sqrt(float(query.shape[1]))
    qk = softmax(qk)
    out = qk @ value
    return out


print(
    attention(
        torch.tensor([[1.0, 2.0], [3.0, 4.0]]),
        torch.tensor([[1.0, 1.0], [0.0, 0.0]]),
        torch.tensor([[2.0, 2.0], [3.0, 3.0]]),
    )
)

print(
    attention(
        torch.tensor([[1.0, 0.0, 1.0], [0.0, 1.0, 1.0]]),
        torch.tensor([[1.0, 1.0, 0.0], [0.0, 0.0, 1.0]]),
        torch.tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]),
    )
)


def mse(x: Tensor, y: Tensor) -> float:
    return float(torch.mean(torch.pow(x - y, torch.tensor(2))))


# print(mse(torch.tensor([1.0, 2.0, 3.0]), torch.tensor([1.0, 2.5, 3.5])))
# print(mse(torch.tensor([[3.0, -3.0, 0.0], [1.0, 0.0, 0.0]]), torch.tensor([[3.0, -2.5, 0.5], [1.0, -1.0, 1.0]])))
