# Core

Personal dotfiles managed via **Nix flakes + Home Manager** (standalone, not NixOS). Single-user macOS (`aarch64-darwin`).

## Entry points
- `flake.nix` — flake root. Defines two `homeConfigurations`: `home` and `work`, both built by `mkProfile`. Declares an overlay exposing custom `pkgs/*` derivations and pinned flake inputs (`crit`, `peon-ping`, `serena`, `learning-opportunities`). Formatter = `nixfmt-tree`.
- `home.nix` — top-level Home Manager config. Sets username/homeDirectory/stateVersion, the `hm*` shell aliases, and the module import list split into `commonImports` (all profiles) + `profileImports` (per-profile `home`/`work`).

## Source map
- `dotfiles/<tool>/default.nix` — one Home Manager module per tool (~60). This is where ~all config lives. See `mem:conventions` for the module pattern and how to add one.
- `pkgs/<name>/default.nix` — custom packages (shell/python scripts wrapped as derivations), surfaced through the flake overlay. See `mem:conventions`.
- `modules/homebrew/` — custom HM module exposing declarative `homebrew.formulae` / `homebrew.taps` options (set per-tool in the `dotfiles/*` modules); reconciled against installed brew state at activation by `pkgs/homebrew-sync`. Imported directly in `flake.nix` `mkProfile` modules. No `Brewfile` is used (the legacy one was removed).
- `scripts/` — `activate-profile.sh` (bootstrap), `calendar-events.js`.
- `.github/workflows/build.yml` — CI; builds both profiles' activationPackages, pushes to cachix `brianhicks-dotfiles`.

## Invariants
- Two profiles only: `home`, `work`. Profile chosen at activation, passed via `extraSpecialArgs.profile`; modules branch on `specialArgs.profile`.
- System is hardcoded `aarch64-darwin` for the home configs (the dev-shell/formatter uses `flake-utils.eachDefaultSystem`).
- Domains: tech/build → `mem:tech_stack`; run/build/format commands → `mem:suggested_commands`; code style & how to add modules/pkgs → `mem:conventions`; what to run before done → `mem:task_completion`.
