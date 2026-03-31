import { readdir, stat } from "node:fs/promises";

export default {
  description:
    "Get projects where files have been written since the last agent review.",
  async execute() {
    return await reportOutdated();
  },
};

async function reportOutdated() {
  const base = "1 Projects";
  const paths = await readdir(base, {
    recursive: true,
  });

  const projects = new Map();
  for (const path of paths) {
    const project = path.split("/")[0];
    const pathStat = await stat(`${base}/${path}`);

    if (pathStat.isDirectory()) continue;

    const current = projects.get(project) ?? {
      agentReview: undefined,
      files: new Map(),
    };

    if (path === `${project}/Agent Review.md`) {
      current.agentReview = pathStat.mtime;
    } else {
      current.files.set(path, pathStat.mtime);
    }

    projects.set(project, current);
  }

  const out = [];

  for (const [project, ages] of projects.entries()) {
    const newerFiles =
      Map.groupBy(
        ages.files,
        ([, age]) => !ages.agentReview || age >= ages.agentReview,
      ).get(true) || [];

    if (newerFiles.length) {
      out.push(`## ${project} (agent review: ${ages.agentReview ?? "never"})`);
      out.push("");
      out.push(
        `- Main file should exist at ${base}/1 Projects/${project}/${project.split(" ").slice(1).join(" ")}.md`,
      );
      if (ages.agentReview) {
        out.push(
          `- Agent review should be at ${base}/1 Projects/${project}/Agent Review.md`,
        );
      }
      out.push("");
      out.push("Updated files since last agent review:");
      out.push("");
      console.log();
      for (const [path, date] of newerFiles) {
        out.push(` - ${base}/${path}: updated ${date}`);
      }
      out.push("");
    }
  }

  return out.join("\n");
}
