class ModelParser:
    def __init__(self, subs):
        self.subscribers = subs
        self.graph = None

    def session_start(self):
        self.graph = dict()

    def session_end(self):
        # self.graph = None
        pass

    def handle(self, data):
        lines = data.split('\n')
        for (i, j) in zip(lines, lines[1:]):
            if j.startswith('conn'):
                node = i.split(' ')[0]
                nodes = j.split(' ')[-1].split(',')
                self.graph[node] = [] if '' in nodes else sorted(nodes)
        self.graph = dict(sorted(self.graph.items()))
        self.notify()

    def notify(self):
        for sub in self.subscribers:
            sub.model_changed(self)