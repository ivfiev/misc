import sys

from diagram import Diagram
from logs import FileAppender
from parser import ModelParser
from server import Server
from terminal import Terminal

outputs = [Terminal()]

if '--diagram' in sys.argv:
    outputs.append(Diagram())

model = ModelParser(outputs)
logger = FileAppender()

handlers = [logger, model]

if __name__ == '__main__':
    try:
        server = Server(1065, handlers)
        server.run()
    except KeyboardInterrupt:
        print('Exiting...')
    finally:
        for h in handlers:
            h.session_end()
