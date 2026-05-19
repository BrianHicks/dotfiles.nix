---
description: Find gaps in important/upcoming projects.
agent: schedule-planner
---

Your job is to find gaps in projects to be filled in for the coming week or month. You have access to the project files in Obsidian, including tasks.

Here's the common procedure.

1. Pull in the projects by running the `outdated-projects` tool. That will include the projects that need to be updated, as well as the ones that are currently up to date.
2. Anything in a `Tasks` directory will be pretty light. The titles are the tasks. The `status` metadata says whether it's ready, working, or done.
3. Spin up sub-agents *with only a single project at once* with the review command given below. Do this for every project, not just outdated ones.
4. Coalesce the sub-agent output into a list of recommendations.

Also fine to recommend projects to be created in Obsidian based on other info you see.

You do not need to "explore" anywhere—just invoke the sub-agents for each project and then coalesce.

Otherwise, just make sure to double-check that you're not getting projects confused. The emoji is the only key here and some projects have tasks with similar names otherwise. That's why you're spinning up sub-agents: it helps a lot with that confusion.

If the user asks afterwards, work together to create tasks. Again, title is the filename. Status is probably "Ready" in this context. If something needs to wait, use the `defer` metadata with an ISO8601-formatted date.

## Review Command

```
---
agent: project-reviewer
---

Review the project at PROJECT PATH, specifically these files which were updated since your last review:

- FILE 1.md
- FILE 2.md

These tasks are outstanding:

- Some task
- Some other task

## Your Task

1. Read the file previous review agents may have written for context in `Agent Review.md`

2. Read the project's main file (the Markdown file matching the directory name in the project directory) to understand:
   - The ideal outcome and deadline
   - Any existing tasks or action items

3. Analyze the changed files, agent review, and main file for:
   - Language indicating missing tasks: "it would be nice/good to…", "I should…", "TODO", markdown-style todo lists, etc.
   - Whether the deadline is coming up and if the project needs prioritization
   - Whether the outcome has been achieved or if the project is too ambitious
   - Any decomposition opportunities if the project is too large
   - Warning signs that the project has stalled or been abandoned
   - Any resources which would be pulled into a PARA resource folder, if the project is complete or very nearly so.

5. Update the context file for future invocations.

It is OK if the project seems up to date. Limit your recommendations to concrete things found in the project instead of giving general advice.

Summarize the project for the calling agent, including any outstanding tasks noted in update files. Cover everything but be as brief as possible.
```
