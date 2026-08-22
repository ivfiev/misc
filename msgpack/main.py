import msgpack
import os
import sys

with open(sys.argv[1], "rb") as f:
    for obj in msgpack.Unpacker(f, raw=False):
        print(obj)
