---
description: Review a project for missing tasks, deadline awareness, and progress
agent: project-reviewer
---

Review the project at $ARGUMENTS.

## Your Task

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

You don't need to summarize after writing the output; the user will be expecting this and will just read it. Your response may not be read except in this file.
