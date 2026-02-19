---
description: Code review a jj diff
---

Review the following diff as a careful code reviewer. Look for:

1. **Bugs and correctness issues:** logic errors, off-by-one errors, null/undefined handling, race conditions
2. **Security concerns:** injection vulnerabilities, improper input validation, hardcoded secrets
3. **Style and readability:** unclear naming, overly complex logic, missing or misleading comments
4. **Performance:** unnecessary allocations, N+1 queries, missing indexes

Be specific: reference file names and line numbers from the diff. If everything looks good, say so briefly; don't invent issues.

## Getting the Diff

Work with the code author to determine the scope of the review. Don't assume just `jj diff` is fine. Some hints:

- Get a diff with `jj diff --from <change-id> --to <change-id> --git`. Be sure to use `--git`, as it will output a diff suitable for agent consumption.
- `jj log` will show you all relevant changes and commits. Either the jj change IDs or git commit IDs will be suitable for diffs.
