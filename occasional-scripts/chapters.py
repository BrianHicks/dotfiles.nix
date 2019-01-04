#!/usr/bin/env python
from __future__ import print_function, unicode_literals
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('num_chapters', type=int)
args = parser.parse_args()

task_templates = [
    "read chapter {}",
    "make chapter summary for chapter {}",
    "make flashcards for chapter {}",
]

for i in range(1, args.num_chapters+1):
    for template in task_templates:
        print(template.format(i))
