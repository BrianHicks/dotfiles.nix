---
description: Refresh agent context for projects.
agent: project-reviewer
---

Review any projects under `1 Projects` which have files newer than the newest `Agent Review.md`.

## For Each Project

1. Read the file previous review agents may have written for context.

2. Read the project's main file (the Markdown file matching the directory name in the project directory) to understand:
   - The ideal outcome and deadline
   - Any existing tasks or action items

3. Read and analyze any updates you haven't seen in from the Updates/ folder (may not exist, which is OK)
   - For efficiency's sake, you should generally avoid re-reading files which have not changed since the last agent review.

4. Analyze the project for:
   - Language indicating missing tasks: "it would be nice/good to…", "I should…", "TODO", etc.
   - Whether the deadline is coming up and if the project needs prioritization
   - Whether the outcome has been achieved or if the project is too ambitious
   - Any decomposition opportunities if the project is too large
   - Any resources which would be pulled into a PARA resource folder, if the project is complete or very nearly so.

5. Update the context file for future invocations.

## Output Format

Write your review to the context file in this format:

```
## Summary
[Brief overview of project status]

## Outstanding Tasks
[Any suggested tasks based on your analysis]

## Deadline
[Is the deadline realistic? Is the project on track?]

## Outcome
[Has the outcome been achieved? Is it still relevant?]

## Decomposition Opportunities
[Can this be split into smaller projects?]

## Scratch Pad
[Your thinking, notes, and observations]
```

You don't need to summarize after writing the output; the user will be expecting this and will just read it. Your response may not be read except in this file.

Be thorough but focused. Your goal is to help the user identify what they might be missing or should reconsider. Do not comment on project organization; these can get messy and it's OK.
