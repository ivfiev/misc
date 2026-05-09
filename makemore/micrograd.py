import math
import random
import numpy as np
import matplotlib.pyplot as plt


class Value:
    def __init__(self, data, _children=(), _op=""):
        self.data = data
        self.grad = 0.0
        self._children = _children
        self._op = _op

    def _name(self):
        for name, val in globals().items():
            if val is self:
                return name
        return None

    def __repr__(self):
        return f"{self._name()}: V[{self.data}], G[{self.grad}]"

    def __add__(self, other):
        return Value(self.data + other.data, (self, other), "+")

    def __sub__(self, other):
        return Value(self.data - other.data, (self, other), "-")

    def __mul__(self, other):
        return Value(self.data * other.data, (self, other), "*")

    def tanh(self):
        return Value(math.tanh(self.data), (self,), "tanh")

    def backward(self, _grad=1.0):  # dL/dSelf
        self.grad += _grad
        if len(self._children) == 1:
            (a,), op = self._children, self._op
            if op == "tanh":
                a.backward((1 - self.data**2) * _grad)
        elif len(self._children) == 2:
            (a, b), op = self._children, self._op
            if op == "+":
                a.backward(_grad)
                b.backward(_grad)
            elif op == "-":
                a.backward(_grad)
                b.backward(-_grad)
            elif op == "*":
                a.backward(_grad * b.data)
                b.backward(_grad * a.data)


class Neuron:
    def __init__(self, n):
        self.ws = [Value(random.normalvariate(sigma=0.1)) for _ in range(n)]
        self.b = Value(random.normalvariate(sigma=0.1))
        self.n = n

    def __call__(self, xs):
        out = Value(0.0)
        for w, x in zip(self.ws, xs):
            out = out + w * x
        out = out + self.b
        out = out.tanh()
        return out

    def zero_grad(self):
        for w in self.ws:
            w.grad = 0.0
        self.b.grad = 0.0

    def step(self, lr):
        for w in self.ws:
            w.data -= lr * w.grad
        self.b.data -= lr * self.b.grad


class Layer:
    def __init__(self, n, m):
        self.ns = [Neuron(n) for _ in range(m)]
        self.n = n
        self.m = m

    def __call__(self, xs):
        return [n(xs) for n in self.ns]

    def zero_grad(self):
        for n in self.ns:
            n.zero_grad()

    def step(self, lr):
        for n in self.ns:
            n.step(lr)


class MLP:
    def __init__(self, ns):
        self.ls = [Layer(n, m) for n, m in zip(ns, ns[1:])]

    def __call__(self, xs):
        for l in self.ls:
            if len(xs) != l.n:
                raise Exception("incompatible dimensions")
            xs = l(xs)
        return xs if len(xs) > 1 else xs[0]

    def zero_grad(self):
        for l in self.ls:
            l.zero_grad()

    def step(self, lr):
        for l in self.ls:
            l.step(lr)


mlp = MLP([3, 4, 4, 1])

xs = [
    [Value(2.0), Value(3.0), Value(-1.0)],
    [Value(3.0), Value(-1.0), Value(0.5)],
    [Value(0.5), Value(1.0), Value(1.0)],
    [Value(1.0), Value(1.0), Value(-1.0)],
]
ys = [Value(1.0), Value(-1.0), Value(-1.0), Value(1.0)]

for i in range(200):
    yhat = [mlp(x) for x in xs]
    loss = Value(0.0)
    for y, yh in zip(ys, yhat):
        loss = loss + (y - yh) * (y - yh)
    mlp.zero_grad()
    loss.backward()
    mlp.step(0.1)
    print(loss * Value(1.0 / len(xs)))
