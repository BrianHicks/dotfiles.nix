---
description: Code review a jj diff
argument-hint: Changes to review (optional)
---

Review the following diff as a careful code reviewer. Look for:

1. **Bugs and correctness issues:** logic errors, off-by-one errors, null/undefined handling, race conditions
2. **Security concerns:** injection vulnerabilities, improper input validation, hardcoded secrets
3. **Style and readability:** unclear naming, overly complex logic, missing or misleading comments
4. **Performance:** unnecessary allocations, N+1 queries, missing indexes

Be specific: reference file names and line numbers from the diff. If everything looks good, say so briefly; don't invent issues.

## Getting the Diff

If there is a change ID or description of a range below, review that. Otherwise, look in `jj log` and prompt the user for what they'd like reviewed.

Get a diff with `jj diff --revision <change-id> --git` or `jj diff --from <change-id> --to <change-id> --git`. Be sure to use `--git`, as it will output a diff suitable for agent consumption.

The user described the commits they want you to start with (if any) in this way:

```
$ARGUMENTS
```
