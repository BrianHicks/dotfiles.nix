import { readdir, stat } from "node:fs/promises";

export default {
  description:
    "Get projects where files have been written since the last agent review.",
  async execute() {
    return await reportOutdated();
  },
};

interface Ages {
  agentReview: Date | undefined;
  files: Map<string, Date>;
}

async function reportOutdated() {
  const base = "1 Projects";
  const paths = await readdir(base, {
    recursive: true,
  });

  const projects = new Map<string, Ages>();
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

  const outdated = [];
  const upToDate = [];

  for (const [project, ages] of projects.entries()) {
    const newerFiles =
      Map.groupBy(
        ages.files,
        ([, age]) => !ages.agentReview || age >= ages.agentReview,
      ).get(true) || [];

    if (newerFiles.length) {
      const out = [
        `## ${project} (agent review: ${date(ages.agentReview)})`,
        "",
        `- Main file should exist at ${base}/${project}/${project.split(" ").slice(1).join(" ")}.md`,
        `- Agent review ${ages.agentReview ? "exists" : "should be created at"} at ${base}/${project}/Agent Review.md`,
        "",
        "Updated files since last agent review:",
        "",
      ];
      for (const [path, updatedAt] of newerFiles) {
        out.push(` - ${base}/${path}: updated ${date(updatedAt)}`);
      }

      outdated.push(out.join("\n"));
    } else {
      upToDate.push(project);
    }
  }

  return [
    "## Summary",
    `Outdated projects: ${outdated.length}`,
    `Up to date projects: ${upToDate.length}`,
    "",
    "## Up-to-date Projects",
    upToDate.length ? upToDate.join(", ") : "None!",
    "",
    "## Outdated Projects",
    outdated.length ? outdated.join("\n\n") : "None!",
  ].join("\n");
}

function date(d: Date | undefined) {
  return d ? d.toISOString() : "never";
}

console.log(await reportOutdated());
