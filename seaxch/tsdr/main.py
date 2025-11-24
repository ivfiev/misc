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
from PIL import Image
import numpy as np
import torch
from torch import Tensor, topk
from sentence_transformers import SentenceTransformer, SimilarityFunction, util


DIR = "/tmp/tsdr"
SOCKET_PATH = f"{DIR}/tsdr.sock"

text_model = SentenceTransformer(
    "all-mpnet-base-v2",
    similarity_fn_name=SimilarityFunction.DOT_PRODUCT,
)

os.environ["TORCH_ROCM_AOTRITON_ENABLE_EXPERIMENTAL"] = "1"
image_model = SentenceTransformer("clip-ViT-L-14")


def touch(path: str, type: str = "file"):
    if type == "file":
        pathlib.Path(path).touch(exist_ok=True)
    if type == "dir":
        os.makedirs(path, exist_ok=True)


def log(msg: str | Exception):
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
        text_model.encode_document(
            words, normalize_embeddings=True, convert_to_tensor=True
        ),
    )
    similarity_scores = text_model.similarity(query, embeddings)[0]
    _, indices = topk(similarity_scores, k=math.ceil(len(words) / 10))
    return list(words[i] for i in indices)


def parse_args(cmd: str):
    try:
        args = json.loads(cmd)
        if args.get("cmd") not in ["t2t", "t2i", "i2i", "status"]:
            raise ValueError("invalid 'cmd'")
        if args["cmd"] in ["t2t", "i2i", "t2i"] and not all(
            [
                isinstance(args.get("path"), str),
                isinstance(args.get("query"), str),
                isinstance(args.get("topk"), int),
                len(args.get("path", "")) > 0,
                len(args.get("query", "")) > 0,
                1 <= args.get("topk", 0) <= 100,
            ]
        ):
            raise ValueError(f"invalid '{args["cmd"]}' args")
        return args
    except json.JSONDecodeError:
        raise ValueError("invalid json payload")


@log_time
def embed_text(path: str, emb_path: str):
    log(f"embedding {path} into {emb_path}")
    with open(path, "r") as file:
        corpus: list[dict] = json.loads(file.read())
        texts = list(entry["text"] for entry in corpus)
        ids = list(entry["id"] for entry in corpus)
        if os.path.exists(emb_path):
            data = np.load(emb_path)
            embeddings = torch.from_numpy(data["embeddings"]).cuda()
            emb_ids = data["ids"]
            emb_texts = data["texts"]
            set_ids = set(emb_ids)
            new_entries = []
            for i in range(len(ids)):
                if ids[i] not in set_ids:
                    new_entries.append({"id": ids[i], "text": texts[i]})
            log(f"appending [{len(new_entries)}] new text embeddings")
            new_texts = [e["text"] for e in new_entries]
            new_embeddings = cast(
                Tensor,
                text_model.encode(
                    new_texts,
                    convert_to_tensor=True,
                    normalize_embeddings=True,
                ),
            )
            log(f"old embeddings [{embeddings.size()}]")
            log(f"new embeddings [{new_embeddings.size()}]")
            embeddings = torch.cat([embeddings, new_embeddings], dim=0)
            log(f"final embeddings [{embeddings.size()}]")
            emb_ids = np.concatenate(
                [emb_ids, np.array([e["id"] for e in new_entries])]
            )
            emb_texts = np.concatenate([emb_texts, np.array(new_texts)])
            assert len(emb_ids) == embeddings.shape[0]
            assert len(emb_texts) == embeddings.shape[0]
            np.savez(
                emb_path, embeddings=embeddings.cpu(), ids=emb_ids, texts=emb_texts
            )
        else:
            embeddings = cast(
                Tensor,
                text_model.encode_document(
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
def embed_images(path: str, emb_path: str):
    log(f"embedding {path} into {emb_path}")
    with open(path, "r") as file:
        corpus: list[dict] = json.loads(file.read())
        paths = list(entry["path"] for entry in corpus)
        ids = list(entry["id"] for entry in corpus)
        if os.path.exists(emb_path):
            data = np.load(emb_path)
            embeddings = torch.from_numpy(data["embeddings"]).cuda()
            emb_ids = data["ids"]
            set_ids = set(emb_ids)
            new_entries = []
            for i in range(len(ids)):
                if ids[i] not in set_ids:
                    new_entries.append({"id": ids[i], "path": paths[i]})
            log(f"appending [{len(new_entries)}] new image embeddings")
            images = []
            for entry in new_entries:
                with Image.open(entry["path"]) as img:
                    images.append(img.copy())
            new_embeddings = cast(
                Tensor,
                image_model.encode(
                    images,  # pyright: ignore
                    convert_to_tensor=True,
                    normalize_embeddings=True,
                ),
            )
            log(f"old embeddings [{embeddings.size()}]")
            log(f"new embeddings [{new_embeddings.size()}]")
            embeddings = torch.cat([embeddings, new_embeddings], dim=0)
            log(f"final embeddings [{embeddings.size()}]")
            emb_ids = np.concatenate(
                [emb_ids, np.array([e["id"] for e in new_entries])]
            )
            assert len(emb_ids) == embeddings.shape[0]
            np.savez(emb_path, embeddings=embeddings.cpu(), ids=emb_ids)
        else:
            log(f"embedding [{len(paths)}] new images")
            images = []
            for path in paths:
                with Image.open(path) as img:
                    images.append(img.copy())
            embeddings = cast(
                Tensor,
                image_model.encode(
                    images,  # pyright: ignore
                    convert_to_tensor=True,
                    normalize_embeddings=True,
                ),
            )
            np.savez(
                emb_path,
                embeddings=embeddings.cpu(),
                ids=np.array(ids),
            )


@log_time
def t2t(path: str, query: str, k: int) -> list[dict]:
    path = f"{DIR}/{path.strip("/")}"
    if not os.path.exists(path):
        raise FileNotFoundError(f"file {path} does not exist")
    emb_path = f"{path}.npz"
    if not os.path.exists(emb_path) or os.path.getmtime(path) > os.path.getmtime(
        emb_path
    ):
        embed_text(path, emb_path)
    log(f"querying [{path}] for '{query}'")
    data = np.load(emb_path)
    embeddings = torch.from_numpy(data["embeddings"]).cuda()
    ids = data["ids"]
    texts = data["texts"]
    query_emb = cast(
        Tensor,
        text_model.encode_query(
            query, convert_to_tensor=True, normalize_embeddings=True
        ),
    )
    similarity_scores = text_model.similarity(query_emb, embeddings)[0]
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


@log_time
def t2i(path: str, query: str, k: int) -> list[dict]:
    path = f"{DIR}/{path.strip("/")}"
    if not os.path.exists(path):
        raise FileNotFoundError(f"file {path} does not exist")
    emb_path = f"{path}.npz"
    if not os.path.exists(emb_path) or os.path.getmtime(path) > os.path.getmtime(
        emb_path
    ):
        embed_images(path, emb_path)
    log(f"querying [{path}] for '{query}'")
    data = np.load(emb_path)
    embeddings = torch.from_numpy(data["embeddings"]).cuda()
    ids = data["ids"]
    query_emb = cast(
        Tensor,
        image_model.encode(query, convert_to_tensor=True, normalize_embeddings=True),
    )
    scores = util.cos_sim(query_emb, embeddings)[0]
    top = scores.topk(k)
    return sorted(
        [
            {
                "score": float(s),
                "id": ids[i],
            }
            for s, i in zip(top.values, top.indices)
        ],
        key=lambda x: float(x["score"]),
        reverse=True,
    )


@log_time
def i2i(path: str, query_image: str, k: int) -> list[dict]:
    path = f"{DIR}/{path.strip("/")}"
    if not os.path.exists(path):
        raise FileNotFoundError(f"file {path} does not exist")
    if not os.path.exists(query_image):
        raise FileNotFoundError(f"query image [{query_image}] does not exist")
    emb_path = f"{path}.npz"
    if not os.path.exists(emb_path) or os.path.getmtime(path) > os.path.getmtime(
        emb_path
    ):
        embed_images(path, emb_path)
    log(f"querying [{path}] for image '{query_image}'")
    data = np.load(emb_path)
    embeddings = torch.from_numpy(data["embeddings"]).cuda()
    ids = data["ids"]
    log(f"encoding {query_image}...")
    query_emb = cast(
        Tensor,
        image_model.encode(
            Image.open(query_image),  # pyright: ignore
            convert_to_tensor=True,
            normalize_embeddings=True,
        ),
    )
    scores = util.cos_sim(query_emb, embeddings)[0]
    top = scores.topk(k)
    return sorted(
        [
            {
                "score": float(s),
                "id": ids[i],
            }
            for s, i in zip(top.values, top.indices)
        ],
        key=lambda x: float(x["score"]),
        reverse=True,
    )


@log_time
def status() -> dict:
    return {
        "gpu": (
            {
                "name": torch.cuda.get_device_name(),
                "vram": f"{round(torch.cuda.memory_reserved() / 1000000)}M",
            }
            if torch.cuda.is_available()
            else None
        ),
    }


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
                if args["cmd"] == "t2t":
                    result = t2t(args["path"], args["query"], args["topk"])
                    conn.sendall(json.dumps(result).encode())
                elif args["cmd"] == "t2i":
                    result = t2i(args["path"], args["query"], args["topk"])
                    conn.sendall(json.dumps(result).encode())
                elif args["cmd"] == "i2i":
                    result = i2i(args["path"], args["query"], args["topk"])
                    conn.sendall(json.dumps(result).encode())
                elif args["cmd"] == "status":
                    result = status()
                    conn.sendall(json.dumps(result).encode())
                else:
                    log(f"unknown command [{args["cmd"]}], ignoring...")
            except socket.timeout:
                continue
            except Exception as e:
                log(e)
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
