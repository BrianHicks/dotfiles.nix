---
description: Create a daily/weekly/etc schedule
agent: schedule-planner
---

Create a plan for the coming day/week/etc.

1. Look at the projects in `1 Projects` (you have been provided a list of currenctly active projects below; these are the directory names inside there.)
2. Invoke the command included below with the indicated sub-agent.
3. Pull all the agent review for the active projects
4. Work with the user to figure out constraints and goals according to the instructions below.

You do not need to "explore" anywhere—just invoke the sub-agents for each project and then coalesce.

## What the User Wants

$ARGUMENTS

## Current Projects

You can get a current list of outdated projects by running the `outdated-projects` tool.

## Review Command

```
---
agent: project-reviewer
---

Review the project at PROJECT PATH, specifically these files which were updated since your last review:

- FILE 1.md
- FILE 2.md
- FILE 3.md

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
   - Warning signs that the project has stalled or been abandoned
   - Any resources which would be pulled into a PARA resource folder, if the project is complete or very nearly so.

5. Update the context file for future invocations.

Summarize the changes you made to the context files for the calling agent.
```
