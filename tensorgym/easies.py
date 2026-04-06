from torch import Tensor
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
