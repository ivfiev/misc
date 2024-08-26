import os
import threading


class Terminal:
    def __init__(self):
        self.model = None
        self.timer = None

    def model_changed(self, model):
        self.model = model
        self.init_timer()

    def redraw(self):
        os.system('clear')
        nodes = list(self.model.graph.keys())
        nodes.sort()
        for node in nodes:
            print(node, ' -> ', ','.join(self.model.graph[node]))
        self.reset_timer()

    def init_timer(self):
        if self.timer is None:
            timer = threading.Timer(0.100, self.redraw)
            self.timer = timer
            timer.start()

    def reset_timer(self):
        self.timer.cancel()
        self.timer = None
