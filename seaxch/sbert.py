import json
import math
import os
import socket
from sentence_transformers import SentenceTransformer, SimilarityFunction
import numpy as np
from torch import Tensor, topk
from typing import List, Tuple, cast

import torch
import api

import util

model = SentenceTransformer(
    "all-mpnet-base-v2",
    similarity_fn_name=SimilarityFunction.DOT_PRODUCT,
)

DIR = "/tmp/seaxch"


def highlights(query: Tensor, post: str) -> List[str]:
    words = list(set(post.split()))
    embeddings = cast(
        Tensor,
        model.encode_document(words, normalize_embeddings=True, convert_to_tensor=True),
    )
    similarity_scores = model.similarity(query, embeddings)[0]
    _, indices = topk(similarity_scores, k=math.ceil(len(words) / 10))
    return list(words[i] for i in indices)


def query(boards: List[str], query: str, k: int) -> List[dict] | dict:
    print(k)
    path = lambda b: f"{DIR}/{board}.npz"
    for board in boards:
        if not os.path.exists(path(board)):
            raise FileNotFoundError(f"embedding {path(board)} does not exist")
    query_emb = cast(
        Tensor,
        model.encode_query(query, convert_to_tensor=True, normalize_embeddings=True),
    )
    result = []
    for board in boards:
        data = np.load(path(board))
        embeddings = torch.from_numpy(data["embeddings"]).cuda()
        posts = data["posts"]
        tids = data["tids"]
        pids = data["pids"]
        similarity_scores = model.similarity(query_emb, embeddings)[0]
        scores, indices = topk(similarity_scores, k=k)
        result.extend(
            {
                "score": float(s),
                "pid": pids[i],
                "highlights": highlights(query_emb, posts[i]),
            }
            for s, i in zip(scores, indices)
        )
    return sorted(result, key=lambda r: r["score"])


def embed(boards: List[str]):
    for board in boards:
        threads = api.unfile4(DIR, board)
        filtered = []
        for thread in threads:
            thread.posts = [post for post in thread.posts if len(post.text) > 50]
            if len(thread.posts) > 3:
                filtered.append(thread)
        texts, tids, pids = zip(
            *[
                (post.text, thread.id, post.id)
                for thread in filtered
                for post in thread.posts
            ]
        )
        embeddings = cast(
            Tensor,
            model.encode_document(
                list(texts), normalize_embeddings=True, convert_to_tensor=True
            ),
        )
        np.savez(
            f"{DIR}/{board}.npz",
            embeddings=embeddings.cpu(),
            tids=np.array(tids),
            pids=np.array(pids),
            posts=np.array(texts),
        )


def parse_args(cmd: str):
    try:
        args = json.loads(cmd)
        mode = args.get("mode")
        if mode not in ["embed", "query"]:
            raise ValueError("'mode' must be embed/query")
        boards = args.get("boards")
        if (
            not isinstance(boards, list)
            or not (1 <= len(boards) <= 10)
            or any(not isinstance(board, str) or len(board) > 5 for board in boards)
        ):
            raise ValueError("'boards' must contain a list of strings")
        query = args.get("query")
        if mode == "query" and (not query or not isinstance(query, str)):
            raise ValueError("'query' is required")
        topk = args.get("topk")
        if topk is not None and not (0 <= topk <= 100):
            raise ValueError("'topk' is invalid")
        return args
    except json.JSONDecodeError:
        raise ValueError("invalid json cmd")


if __name__ == "__main__":
    SOCKET_PATH = f"{DIR}/seaxch.sock"
    util.touch(DIR, type="dir")
    if os.path.exists(SOCKET_PATH):
        os.remove(SOCKET_PATH)
    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server.bind(SOCKET_PATH)
    server.listen(1)
    os.chmod(SOCKET_PATH, 0o666)
    os.chown(DIR, 1000, 1000)
    os.chown(SOCKET_PATH, 1000, 1000)
    try:
        util.log("starting socket server")
        while True:
            try:
                conn, _ = server.accept()
                cmd = util.recvall(conn)
                args = parse_args(cmd)
                if args["mode"] == "embed":
                    embed(args["boards"])
                    conn.sendall("ok".encode())
                if args["mode"] == "query":
                    result = query(args["boards"], args["query"], args["topk"])
                    conn.sendall(json.dumps(result, indent=2).encode())
            except Exception as e:
                util.log(e)
                err = {"error": str(e)}
                conn.sendall(json.dumps(err).encode())
            finally:
                conn.close()
    finally:
        server.close()
        os.remove(SOCKET_PATH)
