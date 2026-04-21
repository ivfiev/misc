from datasets import load_dataset
from sentence_transformers import SentenceTransformer
import torch
import torch.nn.functional as F
from torch import Tensor, nn, optim
from torch.utils.data import DataLoader, TensorDataset

device = "cuda"

data = load_dataset("rotten_tomatoes")
st = SentenceTransformer("all-mpnet-base-v2")
x_train = st.encode(data["train"][:]["text"], convert_to_tensor=True, normalize_embeddings=True).to(device)
y_train = torch.tensor(data["train"][:]["label"], device=device, dtype=torch.float32).view(-1, 1)
x_test = st.encode(data["test"][:]["text"], convert_to_tensor=True, normalize_embeddings=True).to(device)
y_test = torch.tensor(data["test"][:]["label"], device=device, dtype=torch.float32).view(-1, 1)

model = nn.Sequential(
    nn.Linear(768, 128),
    nn.ReLU(),
    nn.Linear(128, 1),
    nn.Sigmoid(),
).to(device)

loader = DataLoader(TensorDataset(x_train, y_train), batch_size=64, shuffle=True)
adam = optim.Adam(model.parameters(), lr=0.001)

with torch.no_grad():
    preds = model(x_test)
    correct = (((preds > 0.5).float()) == y_test).float().mean()
    print(f"Correct: {correct}")

for e in range(3):
    for x, y in loader:
        preds = model(x)
        loss = F.binary_cross_entropy(preds, y)
        loss.backward()
        adam.step()
        adam.zero_grad()
    with torch.no_grad():
        preds = model(x_test)
        correct = (((preds > 0.5).float()) == y_test).float().mean()
        print(f"Correct: {correct}")
