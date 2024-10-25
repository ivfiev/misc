import signal
import tkinter as tk
import threading
import sys
import time
from math import atan2, cos, sin, pi
from pynput import keyboard


class FadingCircle:
    def __init__(self, canvas, x1, y1, x2, y2, t):
        self.canvas = canvas
        self.x0 = 0
        self.y0 = 0
        self.x1 = x1
        self.y1 = y1
        self.t = t
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
        self.fading_thread = threading.Thread(target=self.fade, daemon=True)
        self.fading_thread.start()

    def _get_color(self):
        red = int(100 - 100 * self.alpha)
        return f'#{red:02x}{red:02x}{red:02x}'

    def fade(self):
        while self.alpha > 0:
            self.alpha -= 0.10
            if self.alpha <= 0:
                self.alpha = 0
            self.canvas.itemconfig(self.circle, fill=self._get_color())
            time.sleep(0.1)
        self.canvas.delete(self.circle)

    def recalc_effective(self, x2, y2, t):
        (x, y) = (self.x1 - x2, self.y1 - y2)
        t = -t
        (x, y) = (x * cos(t) - y * sin(t), x * sin(t) + y * cos(t))
        (x, y) = ((x + 3480.0) / 10.0 - self.size / 2, (3480.0 - y) / 10.0 + self.size / 2)
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
        self.prev = (0.0, 0.0)
        self.angle = 0
        self.angle_offset = 0
        self.hidden = False

    def read_stdin(self):
        while True:
            line = sys.stdin.readline()
            for coords in line.split():
                me = False
                if coords.startswith('ME'):
                    me = True
                    coords = coords[2:]
                [x, y] = map(lambda xy: float(xy), coords.strip('()').split(','))
                if me and (abs(x - self.prev[0]) > 0.5 or abs(y - self.prev[1]) > 0.5):
                    (u0, v0) = (0, 1)
                    (u1, v1) = (x - self.prev[0], y - self.prev[1])
                    t = atan2(u0 * v1 - v0 * u1, u0 * u1 + v0 * v1)
                    for circle in self.circles:
                        circle.recalc_effective(x, y, t + self.angle_offset)
                        circle.place()
                    self.prev = (x, y)
                    self.angle = t
                else:
                    self.draw_circle(x, y, self.prev[0], self.prev[1], self.angle)
            new_circles = []
            for c in self.circles:
                if c.alpha > 0:
                    new_circles.append(c)
            self.circles = new_circles
            time.sleep(0.001)

    def draw_circle(self, x1, y1, x2, y2, t):
        self.circles.append(FadingCircle(self.canvas, x1, y1, x2, y2, t))

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
        self.listener = keyboard.Listener(on_press=self.on_press, on_release=self.on_release)
        self.listener.start()

    def on_press(self, key):
        if hasattr(key, 'char'):
            self.held.add(key.char)
            self.set_offset()

    def on_release(self, key):
        if hasattr(key, 'char'):
            self.held.remove(key.char)
            self.set_offset()
        elif key == keyboard.Key.alt_l:
            self.app.toggle()

    def set_offset(self):
        if 'a' in self.held and 'w' in self.held:
            self.app.angle_offset = -pi / 8
        elif 'd' in self.held and 'w' in self.held:
            self.app.angle_offset = pi / 8
        elif 'a' in self.held:
            self.app.angle_offset = -pi / 4
        elif 'd' in self.held:
            self.app.angle_offset = pi / 4
        elif 's' in self.held:
            self.app.angle_offset = pi
        else:
            self.app.angle_offset = 0


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
