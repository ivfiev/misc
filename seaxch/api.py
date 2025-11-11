import requests
import util
from dataclasses import dataclass
from typing import List, Optional


# types
@dataclass
class Post:
    id: str
    thread_id: str
    subj: str
    text: str
    image_url: Optional[str]
    timestamp: int


@dataclass
class Thread:
    id: str
    url: str
    posts: List[Post]


# apis
def catalog4(board: str) -> List[Thread] | None:
    try:
        resp = requests.get(f"https://a.4cdn.org/{board}/threads.json", timeout=10)
        resp.raise_for_status()
        json = resp.json()
        threads = [
            Thread(
                id=t["no"],
                url=f"https://boards.4chan.org/{board}/thread/{t["no"]}",
                posts=[],
            )
            for page in json
            for t in page["threads"]
        ]
        return threads
    except requests.HTTPError as e:
        util.log(e)
        util.log(f"failed to get catalog /{board}/")
        return None


def thread4(board: str, id: str | int) -> Thread | None:
    try:
        resp = requests.get(f"https://a.4cdn.org/{board}/thread/{id}.json", timeout=10)
        resp.raise_for_status()
        json = resp.json()
        thread = Thread(
            id=str(id),
            url=f"https://boards.4chan.org/{board}/thread/{id}",
            posts=[
                Post(
                    id=str(p["no"]),
                    thread_id=str(id),
                    subj=util.unescape(p.get("sub")),
                    text=util.unescape(p.get("com", "")),
                    timestamp=p["time"],
                    image_url=(
                        f"https://i.4cdn.org/{board}/{p["tim"]}s.jpg"
                        if p.get("ext")
                        else None
                    ),
                )
                for p in json["posts"]
            ],
        )
        return thread
    except requests.HTTPError as e:
        util.log(e)
        util.log(f"failed to get thread /{board}/{id}")
        return None
