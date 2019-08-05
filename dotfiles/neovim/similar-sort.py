#!/usr/bin/env python
from __future__ import print_function
import argparse
import sys
import subprocess
import shlex

# https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Python
def levenshtein(s1, s2):
    if len(s1) < len(s2):
        return levenshtein(s2, s1)

    # len(s1) >= len(s2)
    if len(s2) == 0:
        return len(s1)

    previous_row = range(len(s2) + 1)
    for i, c1 in enumerate(s1):
        current_row = [i + 1]
        for j, c2 in enumerate(s2):
            insertions = previous_row[j + 1] + 1 # j+1 instead of j since previous_row and current_row are one character longer
            deletions = current_row[j] + 1       # than s2
            substitutions = previous_row[j] + (c1 != c2)
            current_row.append(min(insertions, deletions, substitutions))
        previous_row = current_row
    
    return previous_row[-1]


def main(args):
    output = subprocess.check_output(shlex.split(args.command))
    lines = sorted(
        output.decode('utf-8').strip().split('\n'),
        key=lambda line: levenshtein(args.target, line),
    )
    for line in lines:
        print(line)

    return 0


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('command', help='command to run to get sources')
    parser.add_argument('target', help='reference to sort against (more similar matches higher)')
    sys.exit(main(parser.parse_args()))
