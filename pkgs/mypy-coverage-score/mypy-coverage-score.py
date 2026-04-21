#!/usr/bin/env python3
import collections
import re
import subprocess
import tempfile
from os import path

commit_count_by_file = collections.Counter(
    name.replace("/", ".").replace(".py", "")
    for name in subprocess.check_output(["git", "log", "--pretty=format:", "--name-only"])
    .decode("utf-8")
    .strip()
    .split()
    if name.endswith(".py")
)

with tempfile.TemporaryDirectory("mypy-coverage-score") as dir:
    subprocess.check_call(["mypy", ".", "--report", "--txt-report", dir])
    with open(path.join(dir, "index.txt"), "r") as fh:
        report_contents = fh.read()

COVERAGE_RE = re.compile(r"^\| ([^ ]+) *\| +([\d\.]+)% imprecise \|.+?$", re.MULTILINE)

imprecision_by_file: dict[str, float] = {}
for line in report_contents.strip().split("\n"):
    if match := COVERAGE_RE.match(line):
        imprecision_by_file[match[1]] = float(match[2]) / 100.0

ranking = sorted(
    (
        (imprecision_by_file.get(filename, 1.0) * count, imprecision_by_file.get(filename, 1.0), count, filename)
        for filename, count in commit_count_by_file.items()
        if "migration" not in filename
    ),
    reverse=True,
)

for score, imprecision, count, filename in ranking:
    print(f"{score:.2f}\t{filename} ({imprecision * 100:.2f}% imprecision, modified in {count} commits)")

