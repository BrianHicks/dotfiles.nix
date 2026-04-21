import { execFileSync } from "node:child_process";
import { join } from "node:path";

export default {
  description:
    "Get upcoming calendar events from FlowSavvy, grouped by project and week.",
  async execute() {
    const script = join(import.meta.dirname, "flowsavvy-schedule");
    const output = execFileSync(script, ["FlowSavvy", "28"], {
      encoding: "utf-8",
      timeout: 15000,
    });
    return output;
  },
};
