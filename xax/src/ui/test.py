# import signal
# import tkinter as tk
# import threading
# import sys
# import time
# from math import cos, sin, pi, sqrt, asin
# from pynput import mouse
#
# (XT, ZT) = (pi / 2.0, pi / 2.5)
# (X100, Y100) = (250.0, 350.0)
#
#
# def rebase2d(x0, y0, x1, y1, t):
#     (x, y) = (x0 - x1, y0 - y1)
#     (x, y) = (x * cos(t) - y * sin(t), x * sin(t) + y * cos(t))
#     return x, y
#
#
# class Rect:
#     def __init__(self):
#         self.root = tk.Tk()
#         self.root.overrideredirect(True)
#         self.root.geometry(f"{0}x{0}+{0}+{0}")
#         self.root.attributes('-topmost', True)
#         self.canvas = tk.Canvas(self.root, width=0, height=0, bg='blue', highlightthickness=2)
#         self.canvas.pack()
#         self.x, self.y = 0, 0
#         self.width, self.height = 0, 0
#         self.visible = False
#
#     def update(self, x0, y0, z0, x1, y1, z1, yaw, pitch):
#         x, y = rebase2d(x0, y0, x1, y1, yaw)
#         z = z0 - z1
#         dist = sqrt(x ** 2 + y ** 2 + z ** 2)
#         print(dist)
#         xt = asin(x / dist)
#         zt = asin(z / dist)  # + pitch
#         if abs(xt) < XT / 2.0 and abs(zt) < ZT / 2.0:
#             self.visible = True
#             (self.width, self.height) = (X100 * 100.0 / dist, Y100 * 100.0 / dist)
#             (max_x, max_z) = (dist * sin(XT / 2.0), dist * sin(ZT / 2.0))
#             print(x, y, z, max_x)
#             self.x, self.y = 1920 + x / max_x * (xt / XT / 2.0) ** 2 * 1920, 1080 - z / max_z * 1080
#         else:
#             self.visible = False
#
#     def toggle(self):
#         if self.visible:
#             self.canvas.config(width=round(self.width), height=round(self.height))
#             self.root.geometry(f"{round(self.width)}x{round(self.height)}+{round(self.x)}+{round(self.y)}")  # correct for top-left
#             self.root.deiconify()
#         else:
#             self.root.withdraw()
#
#
# class FadingCircle:
#     def __init__(self, canvas, x1, y1, x2, y2, t, col):
#         self.canvas = canvas
#         self.x0 = 0
#         self.y0 = 0
#         self.x1 = x1
#         self.y1 = y1
#         self.t = t
#         self.col = col
#         self.size = 14
#         self.alpha = 1.0
#         self.recalc_effective(x2, y2, t)
#         self.circle = self.canvas.create_oval(
#             self.x0 - self.size / 2,
#             self.y0 - self.size / 2,
#             self.x0 + self.size / 2,
#             self.y0 + self.size / 2,
#             fill=self._get_color(), outline=''
#         )
#         self.fade()
#
#     def _get_color(self):
#         col = int(255 * self.alpha)
#         if self.col == 'T':
#             return f'#{col:02x}0000'
#         if self.col == 'C':
#             return f'#0000{col:02x}'
#         if self.col == 'ME':
#             return f'#00{col:02x}00'
#         return f'#{0:02x}{0:02x}{0:02x}'
#
#     def fade(self):
#         if self.alpha > 0:
#             self.alpha -= 0.20
#             if self.alpha < 0:
#                 self.alpha = 0
#             self.canvas.itemconfig(self.circle, fill=self._get_color())
#             self.canvas.after(100, self.fade)
#         else:
#             self.canvas.delete(self.circle)
#
#     def recalc_effective(self, x2, y2, t):
#         (x, y) = rebase2d(self.x1, self.y1, x2, y2, t)
#         (x, y) = ((x + 4000.0) / 16.0, (4000.0 - y) / 16.0)
#         self.x0 = x
#         self.y0 = y
#
#     def place(self):
#         self.canvas.coords(self.circle,
#                            round(self.x0 - self.size / 2),
#                            round(self.y0 - self.size / 2),
#                            round(self.x0 + self.size / 2),
#                            round(self.y0 + self.size / 2))
#
#
# class Overlay(tk.Tk):
#     def __init__(self):
#         super().__init__()
#         self.prev = (0.0, 0.0, 0.0)
#         self.yaw = 0.0
#         self.pitch = 0.0
#         self.overrideredirect(True)
#         self.attributes('-topmost', True)
#         self.geometry('500x500+40+40')
#         self.config(bg='black')
#         self.canvas = tk.Canvas(self, width=500, height=500, bg='black', highlightthickness=0)
#         self.canvas.pack()
#         self.circles = []
#         self.stop_thread = False
#         self.stdin_thread = threading.Thread(target=self.read_stdin, daemon=True)
#         self.stdin_thread.start()
#         self.hidden = False
#         self.rects = [Rect() for _ in range(1)]
#
#     def read_stdin(self):
#         while True:
#             line = sys.stdin.readline()
#             if line:
#                 entities = line.strip().split(' ')
#                 for entity in entities:
#                     [me, team, x, y, z, yaw, pitch] = entity.split(',')
#                     me = me == '1'
#                     x, y, z, yaw, pitch = map(float, [x, y, z, yaw, pitch])
#                     colour = 'ME' if me else 'C' if team == 'C' else 'T'
#                     if me:
#                         t = pi / 2.0 - yaw / 180 * pi
#                         self.prev = (x, y, z)
#                         self.yaw = t
#                         self.pitch = pitch / 90 * pi
#                         for circle in self.circles:
#                             circle.recalc_effective(x, y, t)
#                             circle.place()
#                         self.draw_circle(x, y, x, y, t, colour)
#                     else:
#                         self.draw_circle(x, y, self.prev[0], self.prev[1], self.yaw, colour)
#                         self.rects[0].update(x, y, z, self.prev[0], self.prev[1], self.prev[2], self.yaw, self.pitch)
#                 self.clear_cirlces()
#             if not line:
#                 time.sleep(0.01)
#
#     def draw_circle(self, x1, y1, x2, y2, t, c):
#         self.circles.append(FadingCircle(self.canvas, x1, y1, x2, y2, t, c))
#
#     def clear_cirlces(self):
#         self.circles = [c for c in self.circles if c.alpha > 0]
#
#     def on_closing(self):
#         self.destroy()
#
#     def toggle(self, show):
#         if show and self.hidden:
#             self.deiconify()
#             self.hidden = False
#         elif not show and not self.hidden:
#             self.withdraw()
#             self.hidden = True
#
#     def toggle_rects(self):
#         for rect in self.rects:
#             rect.toggle()
#         self.after(40, self.toggle_rects)
#
#
# class MouseListener():
#     def __init__(self, overlay):
#         self.overlay = overlay
#         self.listener = mouse.Listener(on_move=self.on_move, on_scroll=self.on_scroll)
#         self.listener.start()
#         self.timer = False
#
#     def on_move(self, x, y):
#         if 40 <= x <= 540 and 40 <= y <= 540:
#             self.overlay.toggle(False)
#         elif not self.timer:
#             self.overlay.toggle(True)
#
#     def on_scroll(self, _, __, ___, ____):
#         self.temp_hide()
#
#     def temp_hide(self):
#         def on_timeout():
#             self.overlay.toggle(True)
#             self.timer = False
#
#         if not self.timer:
#             self.timer = True
#             self.overlay.toggle(False)
#             threading.Timer(3.0, on_timeout).start()
#
#
# if __name__ == "__main__":
#     overlay = Overlay()
#     listener = MouseListener(overlay)
#     signal.signal(signal.SIGINT, lambda x, y: sys.exit(0))
#     overlay.after(100, lambda: overlay.toggle_rects())
#     overlay.mainloop()
