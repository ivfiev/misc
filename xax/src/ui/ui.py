import os
import signal
import tkinter as tk
import threading
import sys
import time


class FadingCircle:
    def __init__(self, canvas, x, y):
        self.canvas = canvas
        self.x = x
        self.y = y
        self.size = 20
        self.alpha = 1.0
        self.circle = self.canvas.create_oval(
            x, y, x + self.size, y + self.size, fill=self._get_color(), outline=''
        )
        self.fading_thread = threading.Thread(target=self.fade, daemon=True)
        self.fading_thread.start()

    def _get_color(self):
        red = int(100 - 100 * self.alpha)
        return f'#{red:02x}{red:02x}{red:02x}'

    def fade(self):
        while self.alpha > 0:
            self.alpha -= 0.04
            if self.alpha <= 0:
                self.alpha = 0
            self.canvas.itemconfig(self.circle, fill=self._get_color())
            time.sleep(0.1)
        self.canvas.delete(self.circle)


class CircleOverlayApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.overrideredirect(True)
        self.attributes('-topmost', True)
        self.geometry('696x696+0+0')
        self.config(bg='grey')
        self.canvas = tk.Canvas(self, width=696, height=696, bg='darkgrey', highlightthickness=0)
        self.canvas.pack()
        self.circles = []
        self.stop_thread = False
        self.stdin_thread = threading.Thread(target=self.read_stdin, daemon=True)
        self.stdin_thread.start()
        self.prev1 = (1.0, 1.0)
        self.prev2 = (0.0, 0.0)

    def read_stdin(self):
        os.set_blocking(sys.stdin.fileno(), False)
        while True:
            line = sys.stdin.readline()
            while line is None:
                line = sys.stdin.readline()
            try:
                for coords in line.split():
                    me = False
                    if coords[0] == 'ME':
                        me = True
                        coords = coords[1:]
                    [x, y] = map(lambda xy: float(xy), coords.strip('()').split(','))
                    x = (x + 3500.0 - 10.0) / 10.0
                    y = (3500.0 - y + 10.0) / 10.0
                    if me:
                        self.prev2 = self.prev1
                        self.prev1 = (x, y)
                    self.draw_circle(int(x), int(y))
            except ValueError:
                continue
            time.sleep(0.01)

    def draw_circle(self, x, y):
        vector = self.prev1[0] - self.prev2[0], self.prev1[1] - self.prev2[1]
        self.circles.append(FadingCircle(self.canvas, x, y))

    def on_closing(self):
        self.destroy()


if __name__ == "__main__":
    app = CircleOverlayApp()
    app.protocol("WM_DELETE_WINDOW", app.on_closing)
    signal.signal(signal.SIGINT, lambda x, y: app.destroy())
    tk_check = lambda: app.after(100, tk_check)
    app.after(100, tk_check)
    app.bind_all("<Control-c>", lambda e: app.destroy())
    app.mainloop()
