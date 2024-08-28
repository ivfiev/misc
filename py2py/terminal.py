import os

from output import Output


class Terminal(Output):
    def __init__(self):
        super().__init__()

    def redraw(self):
        os.system('clear')
        for node, edges in self.model.graph.items():
            print(node, ' -> ', ','.join(edges))
        print(f'\nDead: [{','.join(self.model.dead)}]')
