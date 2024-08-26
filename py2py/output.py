import threading


class Output:
    def __init__(self, debounce=0.0):
        self.model = None
        self.timer = None
        self.debounce = debounce if debounce > 0.0 else None

    def model_changed(self, model):
        self.model = model
        if not self.debounce:
            self.redraw()
        elif not self.timer:
            self.timer_init()

    def timer_init(self):
        self.timer = threading.Timer(self.debounce, self.timer_run)
        self.timer.start()

    def timer_run(self):
        self.redraw()
        self.timer.cancel()
        self.timer = None

    def redraw(self):
        pass
