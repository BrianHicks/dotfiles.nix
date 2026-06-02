# Task Completion

No unit tests exist; correctness = "it evaluates and builds." After a change:

1. **Format**: `nix fmt` (nixfmt-tree) — required for any `.nix` edit.
2. **Build-check** the affected profile(s) without activating:
   - `hmb` (current profile), or
   - `nix build .#homeConfigurations.<home|work>.activationPackage`.
   - If the change touches `commonImports` or `flake.nix`/overlay, build **both** profiles (CI does):
     `nix build .#homeConfigurations.home.activationPackage .#homeConfigurations.work.activationPackage`
3. **Apply**: `hms` activates the config into the live environment. **Agents must NOT run `hms`** — stop and let the user run it when ready. `hmb` / `nix build` are fine for agents to run freely.

CI (`.github/workflows/build.yml`) gates on the both-profiles `nix build` succeeding, so a green local build of both = CI-clean.
