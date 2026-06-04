from lang import *

palindrome = [
    [
        [],
        [
            ["NOT", [19], [19]],  # bit that's 1 everywhere
            ["OR", [who("^"), who("$")], [18]],  # bit that's 1 only in special tokens
        ],
    ],
    [
        [
            [
                ["QUERY", [19]],  # Q = 1, searching for 1
                ["KEY", [who("$")]],  # content/K = 1 if $, else 0
                ["VALUE", slice(len(E), len(P))],  # if match, then copy pos
                ["PROJ", slice(20, len(P))],  # project into 20th
            ],
        ],
        [
            *subtract(20, len(E), len(P)),  # subtract actual pos from mirrored
            ["NOT", [18], [18]],  # now this bit is 0 for special, 1 for common tokens
        ],
    ],
    [
        [
            [
                ["QUERY", slice(20, len(P))],  # looking for this (mirrored) pos
                ["KEY", slice(len(E), len(P))],  # ... in each tokens actual pos
                ["VALUE", slice(0, len(E))],  # copy matching token's identity
                ["PROJ", slice(30, len(E))],  # into 30th
            ]
        ],
        [
            *cmp_one_hot(0, 30, len(E), 40),  # compare one-hot into 40
        ],
    ],
    [
        [],
        [
            ["NOT", [40], [40]],  # non-matches become 1
        ],
    ],
    [
        [
            [
                ["QUERY", [who("^")]],  # Q = 1 for ^, 0 else
                ["KEY", [18]],  # K = non-special tokens, ie broadcast
                ["VALUE", [40]],  # aggregate 1 if non-match, else 0
                ["PROJ", [42]],  #  OR them into 42nd, 0 of palindrome, 1 if not
            ],
        ],
        [
            ["NOT", [42], [42]],
        ],
    ],
    [
        lambda m: m[0][42],
    ],
]


def run_tests():
    print(
        "palindrome",
        all(
            [
                run(palindrome, "^a$") == 1.0,
                run(palindrome, "^aa$") == 1.0,
                run(palindrome, "^bccb$") == 1.0,
                run(palindrome, "^ababa$") == 1.0,
                run(palindrome, "^bcab$") == 0.0,
                run(palindrome, "^ababc$") == 0.0,
                run(palindrome, "^bcc$") == 0.0,
                run(palindrome, "^acaa$") == 0.0,
            ]
        ),
    )
