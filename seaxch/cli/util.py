from concurrent.futures import ThreadPoolExecutor, as_completed
import socket
import bs4
import re
import requests
import warnings
import subprocess
import pathlib
import os
import sys
import time

warnings.filterwarnings("ignore", category=bs4.MarkupResemblesLocatorWarning)


def parallel(f, xs, threads=5):
    with ThreadPoolExecutor(max_workers=threads) as executor:
        futures = {executor.submit(f, x): x for x in xs}
        for future in as_completed(futures):
            yield future.result()


def unescape(text: str) -> str:
    if not text:
        return text
    text = bs4.BeautifulSoup(text, "html.parser").get_text(separator="\n")
    return re.sub(r">>(\d+)(\s*)", "", text)


def log(s):
    print(s, file=sys.stderr)


def progress(ratio: float, header: str):
    hs = round(ratio * 20)
    pc = int(100 * round(ratio, 2))
    print("\r" + " " * (len(header) + 30), end="")
    print(f"\r{header}: [{'#' * hs}{" " * (20 - hs)}] {pc}%", end="")


def download(url, path):
    if os.path.isfile(path):
        return
    try:
        with requests.get(url, stream=True) as resp:
            resp.raise_for_status()
            with open(path, "wb") as file:
                for chunk in resp.iter_content(chunk_size=8192):
                    if chunk:
                        file.write(chunk)
    except requests.HTTPError as e:
        log(e)
        log(f"failed to download {url}")


def render(path: str, resize: str = ""):
    if not os.path.isfile(path):
        log(f"no file to render {path}")
        return
    try:
        if resize:
            magick = subprocess.run(
                f"magick {path} -resize {resize} jpg:-",
                shell=True,
                check=True,
                stdout=subprocess.PIPE,
            )
            icat = subprocess.run(
                f"kitty +kitten icat --align left",
                shell=True,
                check=True,
                input=magick.stdout,
            )
        else:
            subprocess.run(
                f"kitty +kitten icat --align left {path}", shell=True, check=True
            )
    except subprocess.CalledProcessError as e:
        log(e)
        log(f"failed to render {path}")


def touch(path: str, type: str = "file"):
    if type == "file":
        pathlib.Path(path).touch(exist_ok=True)
    if type == "dir":
        os.makedirs(path, exist_ok=True)


def highlight(text: str, color="green") -> str:
    if color == "yellow":
        return f"\033[1;33m{text}\033[0m"
    if color == "purple":
        return f"\033[1;35m{text}\033[0m"
    return f"\033[1;32m{text}\033[0m"


def now() -> float:
    return time.time()


def exec(cmd: str):
    try:
        subprocess.run(cmd, shell=True, check=True)
    except subprocess.CalledProcessError as e:
        log(e)
        log(f"failed to run command [{cmd}]")


def sendrecv(sock: str, data: str) -> str:
    try:
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.connect(sock)
        client.sendall(data.encode())
        client.shutdown(socket.SHUT_WR)
        resp = recvall(client)
        return resp
    finally:
        client.close()


def recvall(sock: socket.socket) -> str:
    data = bytearray()
    while True:
        part = sock.recv(4096)
        if not part:
            break
        data.extend(part)
    return bytes(data).decode()


def delim(c: str, len=72):
    print(highlight(c * len, color="purple"))
