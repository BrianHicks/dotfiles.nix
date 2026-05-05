#!/usr/bin/env python3
import ast
import collections
import re
import signal
import subprocess
from pathlib import Path

signal.signal(signal.SIGPIPE, signal.SIG_DFL)

# Enumerate tracked Python files; "migration" paths are excluded to mirror prior behavior.
ls_output = subprocess.check_output(["git", "ls-files", "*.py"]).decode("utf-8")
py_files = [
    Path(line)
    for line in ls_output.strip().split("\n")
    if line and "migration" not in line
]


def path_to_module(p: Path) -> str:
    parts = list(p.with_suffix("").parts)
    if parts and parts[-1] == "__init__":
        parts.pop()
    return ".".join(parts)


modules: dict[str, Path] = {path_to_module(p): p for p in py_files}


def resolve(name: str) -> str | None:
    parts = name.split(".")
    while parts:
        candidate = ".".join(parts)
        if candidate in modules:
            return candidate
        parts.pop()
    return None


graph: dict[str, set[str]] = {m: set() for m in modules}
for module, file_path in modules.items():
    try:
        tree = ast.parse(file_path.read_text(encoding="utf-8"))
    except (SyntaxError, UnicodeDecodeError):
        continue
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                resolved = resolve(alias.name)
                if resolved and resolved != module:
                    graph[module].add(resolved)
        elif isinstance(node, ast.ImportFrom):
            if node.level > 0:
                base_parts = module.split(".")[: -node.level] if module else []
                if node.module:
                    base_parts.append(node.module)
                base = ".".join(base_parts)
            else:
                base = node.module or ""
            for alias in node.names:
                full = f"{base}.{alias.name}" if base else alias.name
                resolved = resolve(full) or (resolve(base) if base else None)
                if resolved and resolved != module:
                    graph[module].add(resolved)


def pagerank(
    graph: dict[str, set[str]],
    damping: float = 0.85,
    iterations: int = 100,
    tol: float = 1e-8,
) -> dict[str, float]:
    nodes = list(graph)
    n = len(nodes)
    if n == 0:
        return {}
    incoming: dict[str, list[str]] = {node: [] for node in nodes}
    for src, dsts in graph.items():
        for dst in dsts:
            incoming[dst].append(src)
    out_count = {node: len(graph[node]) for node in nodes}
    rank = {node: 1.0 / n for node in nodes}
    for _ in range(iterations):
        dangling = sum(rank[node] for node in nodes if out_count[node] == 0)
        base = (1.0 - damping) / n + damping * dangling / n
        new_rank = {node: base for node in nodes}
        for node in nodes:
            for src in incoming[node]:
                new_rank[node] += damping * rank[src] / out_count[src]
        diff = sum(abs(new_rank[node] - rank[node]) for node in nodes)
        rank = new_rank
        if diff < tol:
            break
    return rank


ranks = pagerank(graph)

# Run mypy --strict and tally errors per module. mypy returns non-zero when
# errors are present, so we don't use check_call here.
mypy_result = subprocess.run(
    ["mypy", "--strict", "."], capture_output=True, text=True
)

ERROR_RE = re.compile(r"^(.+?):\d+:(?:\d+:)? error: ", re.MULTILINE)

errors_by_module: collections.Counter[str] = collections.Counter()
for match in ERROR_RE.finditer(mypy_result.stdout):
    module = path_to_module(Path(match.group(1)))
    if module in modules:
        errors_by_module[module] += 1

total_errors = sum(errors_by_module.values()) or 1

# Scoring: pagerank × error_share — important AND broken bubbles to the top,
# clean modules score 0 and drop off.
#
# Alternative we may revisit: error density (pagerank × errors / LOC).
# Same shape but penalizes concentration of errors rather than volume —
# surfaces small, important, very-broken files; demotes large files where
# a handful of errors are diluted across many lines.
ranking = sorted(
    (
        (
            ranks.get(module, 0.0) * (count / total_errors),
            count,
            count / total_errors,
            ranks.get(module, 0.0),
            module,
        )
        for module, count in errors_by_module.items()
    ),
    reverse=True,
)

for score, count, share, rank_value, module in ranking:
    print(
        f"{score:.6f}\t{module} ({count} errors, {share * 100:.2f}% share, pagerank {rank_value:.4f})"
    )
