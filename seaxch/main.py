import json
from typing import Tuple, List, Optional
import util
import re
import api
from dataclasses import asdict
from functools import reduce
import argparse

DIR = "/tmp/seaxch"


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", type=str, help="mode - load/search", required=True)
    parser.add_argument("--boards", nargs="+", help="list of boards")
    parser.add_argument("--regexes", nargs="+", help="list of regexes")
    parser.add_argument("--min-posts", type=int, help="min posts")
    parser.add_argument("--max-age", type=float, help="post max age hours")
    args = parser.parse_args()
    if args.mode not in ["load", "search"]:
        parser.error("invalid --mode value")
    if args.boards is None:
        parser.error("--boards missing")
    if args.mode == "search" and args.regexes is None:
        parser.error("--regexes missing")
    if args.min_posts and args.min_posts <= 1:
        parser.error("invalid --min-posts value")
    if args.max_age and args.max_age <= 0:
        parser.error("invalid --max-age value")
    return args


def load(boards: List[str]):
    for board in boards:
        load_board(board)


def load_board(board: str):
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


def search(
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
            util.log(f"search failed to access {DIR}/{board}")
    print_results(all_results, regexes)


def print_results(
    all_results: List[Tuple[dict, List[dict]]], regexes: List[re.Pattern]
):
    delim = lambda c: print(c * 72)
    all_results.sort(key=lambda t: len(t[1]))
    for thread, results in all_results:
        op = thread["posts"][0]
        print()
        delim("█")
        print(thread["url"])
        try_render(op.get("image_url"))
        print(op.get("subj") or op.get("text", "")[:69] + "...")
        delim("─")
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
            delim("─")
        print()


def try_render(img_url: Optional[str]):
    if not img_url:
        return
    path = f"{DIR}/{img_url.split('/')[-1]}"
    util.download(img_url, path)
    util.render(path, r"300x300\<")


def main():
    util.touch(f"{DIR}", type="dir")
    args = get_args()
    if args.mode == "load":
        load(args.boards)
    if args.mode == "search":
        search(args.boards, args.regexes, args.min_posts, args.max_age)


if __name__ == "__main__":
    main()
