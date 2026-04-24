import matplotlib

matplotlib.use("GTK4Agg")

import matplotlib.pyplot as plt

plt.figure(figsize=(10, 10))
plt.plot([1, 2, 3, 4], [1, 4, 9, 16])
plt.title("Hyprland test plot")
plt.show()
