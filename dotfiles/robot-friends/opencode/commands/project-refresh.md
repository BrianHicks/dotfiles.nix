---
description: Refresh agent context for projects.
agent: project-reviewer
---

Review any projects under `1 Projects` which are outdated according to the `oudated-projects` tool.

## For Each Project

Launch a sub-agent to deal with each of these in parallel and then summarize the result for the user. Otherwise, context tends to get muddy and boundaries between projects slip.

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

Summarize the changes you made to the context files for the user. When updating files, prefer to use the edit tool instead of completely rewriting them so that it's easier for the user to see what you changed.
