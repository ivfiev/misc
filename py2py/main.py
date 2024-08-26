from logs import FileAppender
from server import Server

logger = FileAppender()

handlers = [logger]

try:
    server = Server(1065, [logger])
    server.run()
finally:
    for h in handlers:
        h.session_end()
