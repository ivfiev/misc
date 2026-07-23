seks = ["B", "G"]
days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]


def cond(xs, *ps):
    n = len(xs)
    k = len(xs)
    for p in ps:
        xs = [x for x in xs if p(x)]
        prob = float(len(xs)) / float(k)
        print(f"{prob:.4f} ({len(xs)} / {k})")
        k = len(xs)


simple = [(x, y) for x in seks for y in seks]
harder = [((x, u), (y, w)) for x in seks for u in days for y in seks for w in days]

print("Simple case:")
cond(
    simple,
    lambda x: "B" in x,
    lambda x: x[0] == "B" and x[1] == "B",
)
print()

print("Tuesday case (B):")
cond(
    harder,
    lambda x: ("B", "Tue") in x,
    lambda x: x[0][0] == "B" and x[1][0] == "B",
)
print()

print("Tuesday case (G):")
cond(
    harder,
    lambda x: ("B", "Tue") in x,
    lambda x: x[0][0] == "G" or x[1][0] == "G",
)
