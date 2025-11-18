import json
from typing import Any, Tuple, List, Optional
import util
import re
import api
from dataclasses import asdict
from functools import reduce
import argparse

DIR = "/tmp/seaxch"


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--mode", type=str, help="mode - load/grep/embed/query", required=True
    )
    parser.add_argument("--boards", nargs="+", help="list of boards")
    parser.add_argument("--regexes", nargs="+", help="list of regexes")
    parser.add_argument("--query", type=str, help="semantic query")
    parser.add_argument("--min-posts", type=int, help="min posts")
    parser.add_argument("--max-age", type=float, help="post max age hours")
    parser.add_argument("--topk", type=int, help="top k semantic results")
    args = parser.parse_args()
    if args.mode not in ["load", "grep", "embed", "query"]:
        parser.error("invalid --mode value")
    if (
        args.boards is None
        or len(args.boards) != len({*args.boards})
        or any(len(b) > 5 or not all("a" <= c <= "z" for c in b) for b in args.boards)
    ):
        parser.error("--boards missing or invalid")
    if args.mode == "search" and args.regexes is None:
        parser.error("--regexes missing")
    if args.mode == "query" and args.query is None:
        parser.error("--query missing")
    if args.min_posts and args.min_posts <= 1:
        parser.error("invalid --min-posts value")
    if args.max_age and args.max_age <= 0:
        parser.error("invalid --max-age value")
    if args.topk and not (1 <= args.topk <= 100):
        parser.error("invalid --topk value")
    return args


def load(boards: List[str]):
    for board in boards:
        catalog = api.catalog4(board)
        if not catalog:
            return
        threads = []
        wip = util.parallel(lambda t: api.thread4(board, t.id), catalog)
        for thread in wip:
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
                        if max_age and post["timestamp"] < util.now() - 3600 * max_age:
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
    path = f"{DIR}/{img_url.split('/')[-1]}"
    util.download(img_url, path)
    util.render(path, r"300x300\<")


def embed(boards: List[str]):
    try:
        util.exec("sudo amdgpu.sh --compute")
        resp = util.sendrecv(
            f"{DIR}/seaxch.sock", json.dumps({"mode": "embed", "boards": boards})
        )
        if resp:
            print(resp)
    except Exception as e:
        util.log(e)
    finally:
        util.exec("sudo amdgpu.sh --low")


def print_query_results(boards: List[str], results: list[dict[str, Any]]):
    threads = {b: api.unfile4(DIR, b) for b in boards}
    for r in results:
        score = r["score"]
        highlights = r["highlights"]
        (thread, post) = next(
            (t, p)
            for ts in threads.values()
            for t in ts
            for p in t.posts
            if p.id == r["pid"]
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


def query(boards: List[str], query: str, topk: int | None):
    try:
        util.exec("sudo amdgpu.sh --compute")
        resp = util.sendrecv(
            f"{DIR}/seaxch.sock",
            json.dumps(
                {"mode": "query", "boards": boards, "query": query, "topk": topk or 5}
            ),
        )
        parsed = json.loads(resp)
        if isinstance(parsed, list):
            print_query_results(boards, parsed)
        else:
            print(resp)
    except Exception as e:
        util.log(e)
    finally:
        util.exec("sudo amdgpu.sh --low")


def main():
    util.touch(f"{DIR}", type="dir")
    args = get_args()
    if args.mode == "load":
        load(args.boards)
    if args.mode == "grep":
        grep(args.boards, args.regexes, args.min_posts, args.max_age)
    if args.mode == "embed":
        embed(args.boards)
    if args.mode == "query":
        query(args.boards, args.query, args.topk)


if __name__ == "__main__":
    main()
    # generic ST container, stats(?), images
