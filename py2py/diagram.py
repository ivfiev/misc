import matplotlib.pyplot as plt
import networkx as nx

from output import Output


class Diagram(Output):
    def __init__(self):
        super().__init__()
        plt.ion()
        plt.show()

    def redraw(self):
        plt.clf()
        G = nx.DiGraph()
        for node in self.model.graph:
            G.add_node(node, label=node)
        for node, edges in self.model.graph.items():
            for edge in edges:
                G.add_edge(node, edge)
        pos = nx.circular_layout(G)
        labels = nx.get_node_attributes(G, 'label')
        nx.draw(G, pos, with_labels=False, node_size=1000, node_color='lightblue', edge_color='gray', font_size=10, arrows=True, arrowstyle='-|>')
        nx.draw_networkx_labels(G, pos, labels, font_size=10, font_family='sans-serif')
        plt.draw()
        plt.pause(0.025)
