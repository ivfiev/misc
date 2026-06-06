from lang import *


def is_palindrome(s: str) -> bool:
    a = FeatureAllocator()
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
                *sub_one_hot(POS, a.POS, len(P)),  # subtract actual pos from mirrored
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
                *eq_one_hot(a.EMB, ID, len(E), CMP),  # compare one-hot into 40
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
    a = FeatureAllocator()
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


def strcmp(s1: str, s2: str) -> bool:
    a = FeatureAllocator()
    A = a.alloc()
    POS = a.alloc(len(P))
    BRO = a.alloc(len(E))
    EQ = a.alloc()
    LT = a.alloc()
    RESULT = a.alloc()
    C = a.alloc()
    code = [
        [
            [],
            [["NOT", [A], [A]]],
        ],
        [
            [
                [
                    # copy pos of | into each token
                    ["QUERY", [A]],
                    ["KEY", [who("|")]],
                    ["VALUE", slice(a.POS, len(P))],
                    ["PROJ", slice(POS, len(P))],
                ],
            ],
            [
                *add_one_hot(POS, a.POS, len(P)),  # add curr pos to |'s pos to obtain opposite
                *lt_one_hot(a.POS, POS, len(P), LT),  # flag if to the left of |
            ],
        ],
        [
            [
                [
                    # copy the twin token embedding
                    ["QUERY", slice(POS, len(P))],
                    ["KEY", slice(a.POS, len(P))],
                    ["VALUE", slice(a.EMB, len(E))],
                    ["PROJ", slice(BRO, len(E))],
                ],
            ],
            [
                *eq_one_hot(a.EMB, BRO, len(E), EQ),  # EQ = 1 if twin matches
            ],
        ],
        [
            [],
            [
                ["NOT", [EQ], [EQ]],  # negate EQ for aggregation
                ["NOT", [who("^")], [C]],  # only want to hear from letters
            ],
        ],
        [
            [
                [
                    # aggregate all !EQs, if >0 then return false else true
                    ["QUERY", [who("^"), who("^")]],
                    ["KEY", [LT, C]],
                    ["VALUE", [EQ]],
                    ["PROJ", [RESULT]],
                ],
            ],
            [],
        ],
        [lambda m: m[0][RESULT]],
    ]
    return run(code, f"^{s1}|{s2}$") + run(code, f"^{s2}|{s1}$") == 0.0  # meh


def reverse(s: str) -> str:
    a = FeatureAllocator()
    A = a.alloc()
    MID = a.alloc(len(P))
    POS = a.alloc(len(P))
    BRO = a.alloc(len(E))
    EQ = a.alloc()
    GT = a.alloc()
    RESULT = a.alloc()
    C = a.alloc()
    code = [
        [
            [],
            [["NOT", [A], [A]]],
        ],
        [
            [
                [
                    # copy pos of $ into each token
                    ["QUERY", [A]],
                    ["KEY", [who("$")]],
                    ["VALUE", slice(a.POS, len(P))],
                    ["PROJ", slice(POS, len(P))],
                ],
                [
                    # copy pos of | into each token
                    ["QUERY", [A]],
                    ["KEY", [who("|")]],
                    ["VALUE", slice(a.POS, len(P))],
                    ["PROJ", slice(MID, len(P))],
                ],
            ],
            [
                *sub_one_hot(POS, a.POS, len(P)),  # sub |'s pos from curr pos to obtain opposite
                *gt_one_hot(a.POS, MID, len(P), GT),  # flag if to the right of |
            ],
        ],
        [
            [
                [
                    # copy the twin token embedding
                    ["QUERY", slice(POS, len(P))],
                    ["KEY", slice(a.POS, len(P))],
                    ["VALUE", slice(a.EMB, len(E))],
                    ["PROJ", slice(BRO, len(E))],
                ],
            ],
            [
                ["AND", [who("?"), GT], [RESULT]],
            ],
        ],
        [
            lambda m: "".join(un_E(u[BRO : BRO + len(E)]) for u in m if u[RESULT] == 1.0),
        ],
    ]
    return run(code, f"^{s}|{str('?' * len(s))}$")


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
    print(
        "strcmp    ",
        all(
            [
                strcmp("a", "a"),
                strcmp("strcmp", "strcmp"),
                not strcmp("hello", "world"),
                not strcmp("a", "b"),
                not strcmp("a", "aa"),
                not strcmp("aa", "a"),
            ]
        ),
    )
    print(
        "reverse   ",
        all(
            [
                reverse("abc") == "cba",
                reverse("a") == "a",
                reverse("hello") == "olleh",
                reverse("xyzab") == "bazyx",
            ]
        ),
    )
