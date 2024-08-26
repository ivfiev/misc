import selectors
import socket


class Server:
    def __init__(self, port, handlers):
        self.selector = selectors.EpollSelector()
        self.connected = set()
        self.port = port
        self.handlers = handlers

    def accept(self, sock):
        if len(self.connected) == 0:
            self.handle(lambda h: h.session_start())
        conn, addr = sock.accept()
        conn.setblocking(False)
        self.selector.register(conn, selectors.EVENT_READ, self.read)
        self.connected.add(conn)

    def read(self, conn):
        try:
            data = conn.recv(4096)
            if data:
                self.handle(lambda h: h.handle(data.decode()))
            else:
                self.disconnect(conn)
        except ConnectionResetError:
            self.disconnect(conn)

    def disconnect(self, conn):
        self.selector.unregister(conn)
        conn.close()
        self.connected.remove(conn)
        if len(self.connected) == 0:
            self.handle(lambda h: h.session_end())

    def handle(self, f):
        for h in self.handlers:
            f(h)

    def run(self):
        host = 'localhost'
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind((host, self.port))
        sock.listen(128)
        sock.setblocking(False)
        self.selector.register(sock, selectors.EVENT_READ, self.accept)
        while True:
            events = self.selector.select()
            for key, mask in events:
                callback = key.data
                callback(key.fileobj)
