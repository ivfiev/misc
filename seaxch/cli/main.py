import datetime
import json
import os
import shutil
import time
from typing import Any, Tuple, List, Optional
import util
import re
import api
from dataclasses import asdict
from functools import reduce
import argparse

DIR = "/tmp/seaxch"
TSDR_DIR = "/tmp/tsdr"


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--mode", type=str, help="mode - load/grep/x2x/status", required=True
    )
    parser.add_argument("--boards", nargs="+", help="list of boards")
    parser.add_argument("--regexes", nargs="+", help="list of regexes")
    parser.add_argument("--query", type=str, help="semantic query")
    parser.add_argument("--min-posts", type=int, help="min posts")
    parser.add_argument("--max-age", type=float, help="post max age hours")
    parser.add_argument("--topk", type=int, help="top k semantic results")
    parser.add_argument("--first", type=int, help="first N threads in catalog")
    args = parser.parse_args()
    if args.mode not in ["load", "grep", "t2t", "t2i", "i2i", "status"]:
        parser.error("invalid --mode value")
    if args.mode in ["load", "grep", "t2t", "t2i", "i2i"] and (
        args.boards is None
        or len(args.boards) != len({*args.boards})
        or any(len(b) > 5 or not all("a" <= c <= "z" for c in b) for b in args.boards)
    ):
        parser.error("--boards missing or invalid")
    if args.mode == "search" and args.regexes is None:
        parser.error("--regexes missing")
    if args.mode in ["t2t", "t2i", "i2i"] and not args.query:
        parser.error("--query missing")
    if args.min_posts and args.min_posts <= 1:
        parser.error("invalid --min-posts value")
    if args.max_age and args.max_age <= 0:
        parser.error("invalid --max-age value")
    if args.topk and not (1 <= args.topk <= 100):
        parser.error("invalid --topk value")
    if args.first and not (1 <= args.first <= 200):
        parser.error("invalid --first value")
    return args


def load(boards: List[str], first: Optional[int] = None):
    for board in boards:
        catalog = api.catalog4(board)
        if not catalog:
            return
        if first:
            catalog = catalog[:first]
        threads = []
        wip = util.parallel(lambda t: api.thread4(board, t.id), catalog)
        for _, thread in wip:
            if not thread:
                continue
            threads.append(thread)
            util.progress(len(threads) / len(catalog), f"Downloading /{board}/ threads")
        path = f"{DIR}/{board}"
        util.touch(path)
        try:
            with open(path, "r+") as file:
                data = file.read()
                file.seek(0)
                old = json.loads(data if data else "[]")
                new = [asdict(t) for t in threads]
                merged = list({t["id"]: t for t in [*old, *new]}.values())
                json.dump(merged, file, indent=2)
                file.truncate()
        except (FileNotFoundError, PermissionError, OSError) as e:
            util.log(e)
            util.log(f"search failed to access {DIR}/{board}")
        print()


def grep(
    boards: List[str],
    patterns: List[str],
    min_posts: Optional[int],
    max_age: Optional[float],
):
    all_results = []
    regexes = [re.compile(regex, re.IGNORECASE) for regex in patterns]
    for board in boards:
        try:
            with open(f"{DIR}/{board}", "r") as file:
                threads = json.loads(file.read())
                for thread in threads:
                    if min_posts and len(thread["posts"]) < min_posts:
                        continue
                    thread_results = []
                    for post in thread["posts"]:
                        if max_age and post["timestamp"] < time.time() - 3600 * max_age:
                            continue
                        text = post.get("text")
                        subj = post.get("subj")
                        if all(
                            regex.findall(text) or subj and regex.findall(subj)
                            for regex in regexes
                        ):
                            thread_results.append(post)
                    if thread_results:
                        all_results.append((thread, thread_results))
        except (FileNotFoundError, PermissionError, OSError) as e:
            util.log(e)
            util.log(f"grep failed to access {DIR}/{board}")
    print_grep_results(all_results, regexes)


def print_grep_results(
    all_results: List[Tuple[dict, List[dict]]], regexes: List[re.Pattern]
):
    all_results.sort(key=lambda t: len(t[1]))
    for thread, results in all_results:
        op = thread["posts"][0]
        print()
        util.delim("█")
        print(thread["url"])
        try_render(op.get("image_url"))
        print(op.get("subj") or op.get("text", "")[:69] + "...")
        util.delim("─")
        for post in results:
            try_render(post.get("image_url"))
            highlighted = reduce(
                lambda text, regex: regex.sub(
                    lambda match: util.highlight(match.group(0)), text
                ),
                regexes,
                post.get("text"),
            )
            print(highlighted)
            util.delim("─")
        print()


def try_render(img_url: Optional[str]):
    if not img_url:
        return
    path = f"{DIR}/{img_url.split('/')[-1]}"  # use ./images/
    util.download(img_url, path)
    util.render(path, r"300x300\<")


def print_query_results(boards: List[str], results: list[dict[str, Any]]):
    threads = {b: api.unfile4(DIR, b) for b in boards}
    results = sorted(results, key=lambda r: r["score"])
    for r in results:
        score = r["score"]
        highlights = r.get("highlights", [])
        (thread, post) = next(
            (t, p)
            for ts in threads.values()
            for t in ts
            for p in t.posts
            if p.id == r["id"]
        )
        util.delim("─")
        print(util.highlight(f"[{round(score, 3)}] {post.url}\n", color="yellow"))
        if post.image_url:
            try_render(post.image_url)
        if post.text:
            highlighted = reduce(
                lambda text, h: text.replace(h, util.highlight(h)),
                highlights,
                post.text,
            )
            print(highlighted)
    util.delim("─")


def sync_t2t(board: str):
    origin_file = f"{DIR}/{board}"
    query_file = f"{TSDR_DIR}/{board}.t2t"
    if not os.path.exists(origin_file):
        raise FileNotFoundError(f"origin file [{origin_file}] does not exist")
    if not os.path.exists(query_file) or os.path.getmtime(
        origin_file
    ) > os.path.getmtime(query_file):
        threads = api.unfile4(DIR, board)
        data = json.dumps(
            [
                {"id": p.id, "text": p.text}
                for t in threads
                for p in t.posts
                if len(p.text) > 20
            ]
        )
        with open(query_file, "w") as qf:
            qf.write(data)


def sync_t2i(board: str):
    origin_file = f"{DIR}/{board}"
    query_file = f"{TSDR_DIR}/{board}.t2i"
    if not os.path.exists(origin_file):
        raise FileNotFoundError(f"origin file [{origin_file}] does not exist")
    if not os.path.exists(query_file) or os.path.getmtime(
        origin_file
    ) > os.path.getmtime(query_file):
        threads = api.unfile4(DIR, board)
        posts_with_images = [
            p for t in threads for p in t.posts if p.image_url is not None
        ]
        util.touch(f"{TSDR_DIR}/images", type="dir")
        wip = util.parallel(
            lambda p: util.download(
                p.image_url, f"{TSDR_DIR}/images/{p.image_url.split('/')[-1]}"
            ),
            posts_with_images,
        )
        posts_ok = []
        for p, ok in wip:
            if ok:
                posts_ok.append(p)
            util.progress(
                len(posts_ok) / len(posts_with_images), f"Downloading /{board}/ images"
            )
        util.log(
            f"Downloaded [{len(posts_ok)}] out of [{len(posts_with_images)}] images"
        )
        data = json.dumps(
            [
                {
                    "id": p.id,
                    "path": f"{TSDR_DIR}/images/{p.image_url.split('/')[-1]}",  # pyright: ignore
                }
                for p in posts_ok
            ]
        )
        with open(query_file, "w") as qf:
            qf.write(data)


def sync_i2i(board: str, query_image: str):
    origin_file = f"{DIR}/{board}"
    query_file = f"{TSDR_DIR}/{board}.x2i"
    query_image_path = f"{DIR}/{query_image}"
    if not os.path.exists(origin_file):
        raise FileNotFoundError(f"origin file [{origin_file}] does not exist")
    if not os.path.exists(query_image_path):
        raise FileNotFoundError(f"query image [{query_image}] does not exist")
    shutil.copy(query_image_path, f"{TSDR_DIR}/images/{query_image}")
    if not os.path.exists(query_file) or os.path.getmtime(
        origin_file
    ) > os.path.getmtime(query_file):
        threads = api.unfile4(DIR, board)
        posts_with_images = [
            p for t in threads for p in t.posts if p.image_url is not None
        ]
        util.touch(f"{TSDR_DIR}/images", type="dir")
        wip = util.parallel(
            lambda p: util.download(
                p.image_url, f"{TSDR_DIR}/images/{p.image_url.split('/')[-1]}"
            ),
            posts_with_images,
        )
        posts_ok = []
        for p, ok in wip:
            if ok:
                posts_ok.append(p)
            util.progress(
                len(posts_ok) / len(posts_with_images), f"Downloading /{board}/ images"
            )
        util.log(
            f"Downloaded [{len(posts_ok)}] out of [{len(posts_with_images)}] images"
        )
        data = json.dumps(
            [
                {
                    "id": p.id,
                    "path": f"{TSDR_DIR}/images/{p.image_url.split('/')[-1]}",  # pyright: ignore
                }
                for p in posts_ok
            ]
        )
        with open(query_file, "w") as qf:
            qf.write(data)


def t2t(boards: List[str], query: str, topk: int | None):
    try:
        # util.exec("sudo amdgpu.sh --compute")
        results = []
        for board in boards:
            sync_t2t(board)
            resp = util.sendrecv(
                f"{TSDR_DIR}/tsdr.sock",
                json.dumps(
                    {
                        "cmd": "t2t",
                        "path": f"{board}.t2t",
                        "query": query,
                        "topk": topk or 5,
                    }
                ),
            )
            parsed = json.loads(resp)
            if isinstance(parsed, list):
                results.extend(parsed)
            else:
                print(resp)
        if results:
            results.sort(key=lambda r: r["score"], reverse=True)
            print_query_results(boards, results)
        else:
            print("no results")
    except Exception as e:
        util.log(e)
    # finally:
    #     util.exec("sudo amdgpu.sh --low")


def t2i(boards: List[str], query: str, topk: int | None):
    try:
        # util.exec("sudo amdgpu.sh --compute")
        results = []
        for board in boards:
            sync_t2i(board)
            resp = util.sendrecv(
                f"{TSDR_DIR}/tsdr.sock",
                json.dumps(
                    {
                        "cmd": "t2i",
                        "path": f"{board}.x2i",
                        "query": query,
                        "topk": topk or 5,
                    }
                ),
            )
            parsed = json.loads(resp)
            if isinstance(parsed, list):
                results.extend(parsed)
            else:
                print(resp)
        if results:
            results.sort(key=lambda r: r["score"], reverse=True)
            print_query_results(boards, results)
        else:
            print("no results")
    except Exception as e:
        util.log(e)
    # finally:
    #     util.exec("sudo amdgpu.sh --low")


def i2i(boards: List[str], query_image: str, topk: int | None):
    try:
        # util.exec("sudo amdgpu.sh --compute")
        results = []
        for board in boards:
            sync_i2i(board, query_image)
            resp = util.sendrecv(
                f"{TSDR_DIR}/tsdr.sock",
                json.dumps(
                    {
                        "cmd": "i2i",
                        "path": f"{board}.x2i",
                        "query": f"{TSDR_DIR}/images/{query_image}",
                        "topk": topk or 5,
                    }
                ),
            )
            parsed = json.loads(resp)
            if isinstance(parsed, list):
                results.extend(parsed)
            else:
                print(resp)
        if results:
            results.sort(key=lambda r: r["score"], reverse=True)
            print_query_results(boards, results)
        else:
            print("no results")
    except Exception as e:
        util.log(e)
    # finally:
    #     util.exec("sudo amdgpu.sh --low")


def status():
    boards = [
        (entry.name, entry.stat().st_mtime)
        for entry in os.scandir(DIR)
        if entry.is_file() and len(entry.name) < 5
    ]
    print(f"Loaded boards: {len(boards)}")
    for board, mtime in boards:
        print(f"- {board} @ {datetime.datetime.fromtimestamp(mtime).strftime("%H:%M")}")
    images = [
        entry.name
        for entry in os.scandir(DIR)
        if entry.is_file() and entry.name.endswith(".jpg")
    ]
    print(f"Loaded images: {len(images)}")
    try:
        resp = util.sendrecv(
            f"{TSDR_DIR}/tsdr.sock",
            json.dumps({"cmd": "status"}),
        )
        status = json.loads(resp)
        gpu = status.get("gpu")
        if gpu:
            print(f"{gpu["name"]}")
            print(f"VRAM: {gpu["vram"]}")
    except FileNotFoundError:
        print("No GPU connected")


if __name__ == "__main__":
    util.touch(f"{DIR}", type="dir")
    args = get_args()
    if args.mode == "load":
        load(args.boards, args.first)
    if args.mode == "grep":
        grep(args.boards, args.regexes, args.min_posts, args.max_age)
    if args.mode == "t2t":
        t2t(args.boards, args.query, args.topk)
    if args.mode == "t2i":
        t2i(args.boards, args.query, args.topk)
    if args.mode == "i2i":
        i2i(args.boards, args.query, args.topk)
    # if args.mode == "i2t":
    #     i2t(args.boards, args.query, args.topk)  meh
    if args.mode == "status":
        status()
