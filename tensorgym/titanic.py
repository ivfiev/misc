import torch
from torch import Tensor, nn, optim
import torch.nn.functional as F
import pandas as pd
import numpy as np

df = pd.read_csv("./data/titanic/Titanic-Dataset.csv")

df["Age"] = df["Age"].fillna(df["Age"].median())
df["Sex"] = df["Sex"].map({"female": 0, "male": 1})
df["Fare"] = np.log1p(df["Fare"])
df["FamilySize"] = df["SibSp"] + df["Parch"]
df["IsAlone"] = (df["FamilySize"] == 0).astype(int)

samples = []
for i in range(100):
    df = df.sample(frac=1).reset_index(drop=True)
    features = ["Pclass", "Sex", "Age", "FamilySize", "IsAlone", "Fare"]
    X = torch.tensor(df[features].values, dtype=torch.float32)
    y = torch.tensor(df["Survived"].values, dtype=torch.float32).view(-1, 1)

    split = int(0.8 * float(X.shape[0]))
    x_train, x_val = X[:split], X[split:]
    y_train, y_val = y[:split], y[split:]

    model = nn.Sequential(
        nn.Linear(6, 32),
        nn.ReLU(),
        nn.Linear(32, 16),
        nn.ReLU(),
        nn.Linear(16, 1),
        nn.Sigmoid(),
    )
    adam = optim.Adam(model.parameters(), lr=0.005)

    for i in range(300):
        loss = F.binary_cross_entropy(model(x_train), y_train)
        loss.backward()
        adam.step()
        adam.zero_grad()
        if i % 100 == 0:
            print(loss.item())

    with torch.no_grad():
        probs = model(x_val)
        samples.append(((probs > 0.5).float() == y_val).float().mean())

samples = np.array(samples)
print("mean:", samples.mean())
print("std:", samples.std())
