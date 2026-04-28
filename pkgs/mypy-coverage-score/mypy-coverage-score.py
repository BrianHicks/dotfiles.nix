#!/usr/bin/env python3
import collections
import re
import signal
import subprocess
import tempfile
from os import path

signal.signal(signal.SIGPIPE, signal.SIG_DFL)

# Get commit history with rename detection
log_output = (
    subprocess.check_output(["git", "log", "-M", "--pretty=format:", "--name-status"])
    .decode("utf-8")
)

path_counts: collections.Counter[str] = collections.Counter()
rename_map: dict[str, str] = {}  # old_path -> current_path

for line in log_output.split("\n"):
    line = line.strip()
    if not line:
        continue
    parts = line.split("\t")
    status = parts[0]

    if status.startswith("R") and len(parts) >= 3:
        old_path, new_path = parts[1], parts[2]
        if old_path.endswith(".py") and new_path.endswith(".py"):
            canonical = rename_map.get(new_path, new_path)
            rename_map[old_path] = canonical
        if new_path.endswith(".py"):
            canonical = rename_map.get(new_path, new_path)
            path_counts[canonical] += 1
    elif len(parts) >= 2:
        file_path = parts[1]
        if file_path.endswith(".py"):
            canonical = rename_map.get(file_path, file_path)
            path_counts[canonical] += 1

# Filter to existing files and convert to module names
commit_count_by_file: collections.Counter[str] = collections.Counter()
for file_path, count in path_counts.items():
    if path.exists(file_path):
        module_name = file_path.replace("/", ".").replace(".py", "")
        commit_count_by_file[module_name] = count

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

