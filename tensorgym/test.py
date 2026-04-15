from datetime import datetime, timezone
import math
import torch


def now():
    return int(datetime.now(timezone.utc).timestamp() * 1000)


d = "cuda"

torch.manual_seed(42)
A = torch.randn((10000, 10000), device=d)
B = torch.randn((10000, 10000), device=d)

start = now()

for i in range(10):
    A = A @ B
    A = A * math.sqrt(1.0 / A.shape[0])
torch.cuda.synchronize()

end = now()
elapsed = end - start

print(A)
print(f"{elapsed} ms")
