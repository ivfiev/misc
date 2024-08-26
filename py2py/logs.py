import os
from datetime import datetime


class FileAppender:
    def __init__(self):
        self.file = None

    def session_start(self):
        self.file = self.init_file()

    def session_end(self):
        if self.file:
            self.file.close()

    def handle(self, data):
        self.file.write(data)

    def init_file(self):
        os.makedirs('./py2py_logs', exist_ok=True)
        filename = datetime.now().strftime("./py2py_logs/logs_%Y-%m-%d-%H:%M:%S")
        file = open(filename, 'a')
        return file
