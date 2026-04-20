---
description: Find gaps in important/upcoming projects.
agent: schedule-planner
---

Your job is to find gaps in projects to be filled in for the coming week or month. You have access to the project files in Obsidian as well as a list of tasks that are currently scheduled through FlowSavvy.

Here's the common procedure:

1. Pull in the projects by running the `outdated-projects` tool. That will include the projects that need to be updated, as well as the ones that are currently up to date.
2. Pull in any areas by running `obsidian base:query file=Areas.base`.
3. For each outdated project, invoke a subagent with the review command in the appendix below to get it up to date. For each up-to-date project, read and summarize `Agent Review.md` in the same way.
4. Compare and contrast the schedule you can get with the `flowsavvy-schedule` command with the outstanding state and tasks that you can see in each project. Cross-reference emoji in the tasks with the emoji associated with projects or areas.

Recommend tasks to be added to FlowSavvy based on the projects, outstanding state, and ideal outcome.

Also fine to recommend projects to be created in Obsidian based on tasks in FlowSavvy.

You do not need to "explore" anywhere—just invoke the sub-agents for each project and then coalesce.

Otherwise, just make sure to double-check that you're not getting projects confused. The emoji is the only key here and some projects have tasks with similar names otherwise.

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

Summarize the project for the calling agent. You should include the current state of the project, the ideal outcome of the project, and any outstanding tasks noted in update files. Cover everything but be as brief as possible.
```
