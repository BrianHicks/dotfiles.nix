#!/usr/bin/env python3
import re
import subprocess
import sys

test_re = re.compile(
    r"(^class (?P<cls>\w+)\(|^\s+def (?P<test>test_.+?)\()", re.MULTILINE
)

files = subprocess.check_output(["fd", "--glob", "*test*.py"]).decode("utf-8").split()

for file in files:
    base = file.replace("/", ".").replace(".py", "")
    print(base)

    with open(file, "r") as fh:
        current_class = None

        for match in test_re.finditer(fh.read()):
            parts = match.groupdict()
            if parts["cls"]:
                current_class = parts["cls"]
                print(f"{base}.{current_class}")
            elif parts["test"]:
                print(f"{base}.{current_class}.{parts['test']}")
            else:
                print("regex misconfiguration: neither cls nor test was set")
                sys.exit(1)
