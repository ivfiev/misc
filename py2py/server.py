import selectors
import socket
import sys

selector = selectors.EpollSelector()
connected = set()
port = 1065
host = 'localhost'


def accept(sock):
    conn, addr = sock.accept()
    conn.setblocking(False)
    selector.register(conn, selectors.EVENT_READ, read)
    connected.add(conn)


def read(conn):
    try:
        data = conn.recv(4096)
        if data:
            print(data.decode(), end='', flush=True)
        else:
            disconnect(conn)
    except ConnectionResetError:
        disconnect(conn)


def disconnect(conn):
    selector.unregister(conn)
    conn.close()
    connected.remove(conn)
    if len(connected) == 0:
        print('<<END_OF_SESSION>>', flush=True)


def run():
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind((host, port))
    sock.listen(128)
    sock.setblocking(False)
    selector.register(sock, selectors.EVENT_READ, accept)
    while True:
        events = selector.select()
        for key, mask in events:
            callback = key.data
            callback(key.fileobj)


if __name__ == '__main__':
    try:
        run()
    except:
        sys.stdout.flush()
