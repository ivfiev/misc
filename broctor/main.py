import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# plt.style.use("dark_background")

np.random.seed(42)

u = 0.07
o = 0.15

dt = 1.0 / 252.0
years = 1.0
n = 10

xi = np.random.normal(size=(252 * int(years), n))
x = ((1 + u) ** dt + o * np.sqrt(dt) * xi).cumprod(axis=0)  # Var = o^2 * 1/252 * 252 = o^2
x = np.concat([np.ones(shape=(1, n)), x], axis=0)

df = pd.DataFrame(x)
df.plot()
plt.show()

print(df)
