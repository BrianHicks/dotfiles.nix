#!/usr/bin/env python
from __future__ import print_function, unicode_literals
import sys

task_templates = [
    "read {}",
    "make chapter summary for {}",
    "make flashcards for {}",
]

for line in sys.stdin.readlines():
    for template in task_templates:
        print(template.format(line.strip()))
