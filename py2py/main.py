from logs import FileAppender
from model import Model
from server import Server
from terminal import Terminal

terminal = Terminal()
diagram = None

model = Model([terminal])
logger = FileAppender()

handlers = [logger, model]

try:
    server = Server(1065, handlers)
    server.run()
except KeyboardInterrupt:
    print('Exiting...')
finally:
    for h in handlers:
        h.session_end()
