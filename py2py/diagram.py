import json
import os
import sys

import matplotlib.animation as animation
import matplotlib.pyplot as plt
import networkx as nx

os.set_blocking(sys.stdin.fileno(), False)
fig, ax = plt.subplots(figsize=(7, 7))


def get_nodes(model):
    s = set()
    for node, edges in model['graph'].items():
        s.update([node, *edges])
    return sorted(list(s))


def update(_):
    line = sys.stdin.readline()
    if not line:
        return
    model = json.loads(line)
    G = nx.DiGraph()
    for node in get_nodes(model):
        G.add_node(node, label=node)
    for node, edges in model['graph'].items():
        for edge in edges:
            G.add_edge(node, edge)
    pos = nx.circular_layout(G)
    ax.clear()
    nx.draw(G, pos, ax=ax, with_labels=True, node_color='lightblue', edge_color='gray', node_size=1000, font_size=10, font_color='black')


ani = animation.FuncAnimation(fig, update, interval=100)

plt.show()
