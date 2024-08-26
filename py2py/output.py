import time


def now():
    return int(time.time() * 1000)


class Output:
    def __init__(self):
        self.model = None
        self.timer = now()

    def model_changed(self, model):
        self.model = model
        if now() >= self.timer:
            self.redraw()
        self.init_timer()

    def init_timer(self):
        if now() >= self.timer:
            self.timer = now() + 100

    def redraw(self):
        pass
