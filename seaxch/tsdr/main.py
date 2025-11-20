import datetime
from functools import wraps
import json
import math
import os
import pathlib
import socket
import sys
import time
from typing import cast
import numpy as np
import torch
from torch import Tensor, topk
from sentence_transformers import SentenceTransformer, SimilarityFunction


DIR = "/tmp/tsdr"
SOCKET_PATH = f"{DIR}/tsdr.sock"

model = SentenceTransformer(
    "all-mpnet-base-v2",
    similarity_fn_name=SimilarityFunction.DOT_PRODUCT,
)


def touch(path: str, type: str = "file"):
    if type == "file":
        pathlib.Path(path).touch(exist_ok=True)
    if type == "dir":
        os.makedirs(path, exist_ok=True)


def log(msg: str):
    print(
        f"[{datetime.datetime.now().strftime("%H:%M:%S.%f")[:-3]}] {msg}",
        file=sys.stderr,
    )


def log_time(func):

    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.perf_counter()
        try:
            return func(*args, **kwargs)
        finally:
            end = time.perf_counter()
            log(f"{func.__name__} took [{end - start:.3f}] seconds")

    return wrapper


def recvall(sock: socket.socket) -> str:
    data = bytearray()
    while True:
        part = sock.recv(4096)
        if not part:
            break
        data.extend(part)
    return bytes(data).decode()


def highlights(query: Tensor, post: str) -> list[str]:
    words = list(set(post.split()))
    embeddings = cast(
        Tensor,
        model.encode_document(words, normalize_embeddings=True, convert_to_tensor=True),
    )
    similarity_scores = model.similarity(query, embeddings)[0]
    _, indices = topk(similarity_scores, k=math.ceil(len(words) / 10))
    return list(words[i] for i in indices)


def parse_args(cmd: str):
    try:
        args = json.loads(cmd)
        if args.get("cmd") not in ["query"]:
            raise ValueError("invalid 'cmd'")
        if args["cmd"] == "query" and not all(
            [
                isinstance(args.get("path"), str),
                isinstance(args.get("query"), str),
                isinstance(args.get("topk"), int),
                len(args.get("path", "")) > 0,
                len(args.get("query", "")) > 0,
                1 <= args.get("topk", 0) <= 100,
            ]
        ):
            raise ValueError("invalid 'query' args")
        return args
    except json.JSONDecodeError:
        raise ValueError("invalid json payload")


@log_time
def embed(path: str, emb_path: str):
    log(f"embedding {path} into {emb_path}")
    with open(path, "r") as file:
        corpus: list[dict] = json.loads(file.read())
        texts = list(entry["text"] for entry in corpus)
        ids = list(entry["id"] for entry in corpus)
        embeddings = cast(
            Tensor,
            model.encode_document(
                texts, convert_to_tensor=True, normalize_embeddings=True
            ),
        )
        np.savez(
            emb_path,
            embeddings=embeddings.cpu(),
            ids=np.array(ids),
            texts=np.array(texts),
        )


@log_time
def query(path: str, query: str, k: int) -> list[dict]:
    path = f"{DIR}/{path.strip("/")}"
    if not os.path.exists(path):
        raise FileNotFoundError(f"file {path} does not exist")
    emb_path = f"{path}.npz"
    if not os.path.exists(emb_path) or os.path.getmtime(path) > os.path.getmtime(
        emb_path
    ):
        embed(path, emb_path)
    log(f"querying [{path}] for '{query}'")
    data = np.load(emb_path)
    embeddings = torch.from_numpy(data["embeddings"]).cuda()
    ids = data["ids"]
    texts = data["texts"]
    query_emb = cast(
        Tensor,
        model.encode_query(query, convert_to_tensor=True, normalize_embeddings=True),
    )
    similarity_scores = model.similarity(query_emb, embeddings)[0]
    scores, indices = topk(similarity_scores, k=min(k, len(ids)))
    return sorted(
        [
            {
                "score": float(s),
                "id": ids[i],
                "highlights": highlights(query_emb, texts[i]),
            }
            for s, i in zip(scores, indices)
        ],
        key=lambda x: float(x["score"]),
        reverse=True,
    )


if __name__ == "__main__":
    touch(DIR, type="dir")
    if os.path.exists(SOCKET_PATH):
        log("removing a dangling socket")
        os.remove(SOCKET_PATH)
    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server.bind(SOCKET_PATH)
    server.listen(1)
    server.settimeout(1)
    os.chmod(SOCKET_PATH, 0o666)
    os.chmod(DIR, 0o777)
    try:
        log("starting socket server")
        while True:
            try:
                conn, _ = server.accept()
                cmd = recvall(conn)
                log(f"message received [{cmd}]")
                args = parse_args(cmd)
                if args["cmd"] == "query":
                    result = query(args["path"], args["query"], args["topk"])
                    conn.sendall(json.dumps(result).encode())
            except socket.timeout:
                continue
            except Exception as e:
                log(str(e))
                err = {"error": str(e)}
                conn.sendall(json.dumps(err).encode())
            finally:
                try:
                    conn.close()
                except NameError:
                    pass
    except KeyboardInterrupt:
        log("shutting down...")
    finally:
        server.close()
        os.remove(SOCKET_PATH)
