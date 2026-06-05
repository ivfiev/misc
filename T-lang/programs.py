from lang import *


def is_palindrome(s: str) -> bool:
    a = Allocator()
    S = a.alloc()
    A = a.alloc()
    POS = a.alloc(len(P))
    ID = a.alloc(len(E))
    CMP = a.alloc()
    RESULT = a.alloc()
    code = [
        [
            [],
            [
                ["NOT", [A], [A]],  # bit that's 1 everywhere
                ["OR", [who("^"), who("$")], [S]],  # bit that's 1 only in special tokens
            ],
        ],
        [
            [
                [
                    ["QUERY", [A]],  # Q = 1, searching for 1
                    ["KEY", [who("$")]],  # content/K = 1 if $, else 0
                    ["VALUE", slice(a.POS, len(P))],  # if match, then copy pos
                    ["PROJ", slice(POS, len(P))],  # project into 20th
                ],
            ],
            [
                *subtract(POS, a.POS, len(P)),  # subtract actual pos from mirrored
                ["NOT", [S], [S]],  # now this bit is 0 for special, 1 for common tokens
            ],
        ],
        [
            [
                [
                    ["QUERY", slice(POS, len(P))],  # looking for this (mirrored) pos
                    ["KEY", slice(a.POS, len(P))],  # ... in each tokens actual pos
                    ["VALUE", slice(a.EMB, len(E))],  # copy matching token's identity
                    ["PROJ", slice(ID, len(E))],  # into 30th
                ]
            ],
            [
                *cmp_one_hot(a.EMB, ID, len(E), CMP),  # compare one-hot into 40
            ],
        ],
        [
            [],
            [
                ["NOT", [CMP], [CMP]],  # non-matches become 1
            ],
        ],
        [
            [
                [
                    ["QUERY", [who("^")]],  # Q = 1 for ^, 0 else
                    ["KEY", [S]],  # K = non-special tokens, ie broadcast
                    ["VALUE", [CMP]],  # aggregate 1 if non-match, else 0
                    ["PROJ", [RESULT]],  #  OR them into 42nd, 0 of palindrome, 1 if not
                ],
            ],
            [
                ["NOT", [RESULT], [RESULT]],
            ],
        ],
        [
            lambda m: m[0][RESULT],
        ],
    ]
    return run(code, f"^{s}$") == 1.0


def count_letter(s: str, c: str) -> int:
    a = Allocator()
    POS = a.alloc(len(P))
    code = [
        [
            [
                [
                    ["QUERY", [who("^")]],
                    ["KEY", [who(c)]],
                    ["VALUE", slice(a.POS, len(P))],
                    ["PROJ", slice(POS, len(P))],
                ],
            ],
            [],
        ],
        [lambda m: int(sum(m[0][POS : POS + len(P)]))],
    ]
    return run(code, f"^{s}$")


def run_tests():
    print(
        "palindrome",
        all(
            [
                is_palindrome("a"),
                is_palindrome("aa"),
                is_palindrome("ababa"),
                is_palindrome("abba"),
                is_palindrome("abbba"),
                is_palindrome("nolemonnomelon"),
                not is_palindrome("hfaksdfhs"),
                not is_palindrome("ababababab"),
            ]
        ),
    )
    print("strawberry", count_letter("strawberry", "r") == 3.0)
