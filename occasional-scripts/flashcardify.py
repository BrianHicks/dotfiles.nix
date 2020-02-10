#!/usr/bin/env python
import sys
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--name", default=None)
args = parser.parse_args()

for line in sys.stdin.read().split("\n"):
    out = line.strip("- ").split("|")
    if args.name is not None:
        out.append(args.name)

    print("\t".join(out))
