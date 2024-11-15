import signal
import tkinter as tk
import threading
import sys
import time
from math import atan2, cos, sin, pi
from pynput import keyboard


class FadingCircle:
    def __init__(self, canvas, x1, y1, x2, y2, t, col):
        self.canvas = canvas
        self.x0 = 0
        self.y0 = 0
        self.x1 = x1
        self.y1 = y1
        self.t = t
        self.col = col
        self.size = 10
        self.alpha = 1.0
        self.recalc_effective(x2, y2, t)
        self.circle = self.canvas.create_oval(
            self.x0 - self.size / 2,
            self.y0 - self.size / 2,
            self.x0 + self.size,
            self.y0 + self.size,
            fill=self._get_color(), outline=''
        )
        self.fade()

    def _get_color(self):
        col = int(255 * self.alpha)
        if self.col == 'T':
            return f'#{col:02x}0000'
        if self.col == 'C':
            return f'#0000{col:02x}'
        if self.col == 'ME':
            return f'#00{col:02x}00'
        return f'#{0:02x}{0:02x}{0:02x}'

    def fade(self):
        if self.alpha > 0:
            self.alpha -= 0.20
            if self.alpha < 0:
                self.alpha = 0
            self.canvas.itemconfig(self.circle, fill=self._get_color())
            self.canvas.after(100, self.fade)
        else:
            self.canvas.delete(self.circle)

    def recalc_effective(self, x2, y2, t):
        (x, y) = (self.x1 - x2, self.y1 - y2)
        t = -t
        (x, y) = (x * cos(t) - y * sin(t), x * sin(t) + y * cos(t))
        (x, y) = ((x + 4000.0) / 20.0 - self.size / 2, (4000.0 - y) / 20.0 + self.size / 2)
        self.x0 = x
        self.y0 = y

    def place(self):
        self.canvas.coords(self.circle,
                           self.x0 - self.size / 2,
                           self.y0 - self.size / 2,
                           self.x0 + self.size,
                           self.y0 + self.size)


class CircleOverlayApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.prev = (0.0, 0.0)
        self.angle = 0.0
        self.overrideredirect(True)
        self.attributes('-topmost', True)
        self.geometry('400x400+40+40')
        self.config(bg='black')
        self.canvas = tk.Canvas(self, width=400, height=400, bg='black', highlightthickness=0)
        self.canvas.pack()
        self.circles = []
        self.stop_thread = False
        self.stdin_thread = threading.Thread(target=self.read_stdin, daemon=True)
        self.stdin_thread.start()
        self.hidden = False

    def read_stdin(self):
        while True:
            line = sys.stdin.readline()
            if line:
                if not line.endswith(',1\n'):
                    continue
                me = False
                [coords, meta] = line.split(':')
                if meta.startswith('1'):
                    me = True
                [x, y, yaw] = map(lambda xy: float(xy), coords.split(','))
                col = 'ME' if meta.startswith('1') else 'T' if 'T' in meta else 'C'
                if me:
                    t = yaw / 180 * pi - pi / 2.0
                    self.prev = (x, y)
                    self.angle = t
                    for circle in self.circles:
                        circle.recalc_effective(x, y, t)
                        circle.place()
                    self.draw_circle(x, y, x, y, t, col)
                else:
                    self.draw_circle(x, y, self.prev[0], self.prev[1], self.angle, col)
            self.circles = [c for c in self.circles if c.alpha > 0]
            if not line:
                time.sleep(0.01)

    def draw_circle(self, x1, y1, x2, y2, t, c):
        self.circles.append(FadingCircle(self.canvas, x1, y1, x2, y2, t, c))

    def on_closing(self):
        self.destroy()

    def toggle(self):
        if self.hidden:
            self.deiconify()
            self.hidden = False
        else:
            self.withdraw()
            self.hidden = True


class KbdListener():
    def __init__(self, app):
        self.held = set()
        self.app = app
        self.listener = keyboard.Listener(on_release=self.on_release)
        self.listener.start()

    def on_release(self, key):
        if key == keyboard.Key.alt_l:
            self.app.toggle()


if __name__ == "__main__":
    print('starting')
    app = CircleOverlayApp()
    listener = KbdListener(app)
    app.protocol("WM_DELETE_WINDOW", app.on_closing)
    signal.signal(signal.SIGINT, lambda x, y: app.destroy())
    tk_check = lambda: app.after(100, tk_check)
    app.after(100, tk_check)
    app.bind_all("<Control-c>", lambda e: app.destroy())
    app.mainloop()
