# Suggested Commands

Profile-aware aliases (defined in `home.nix`; `${profile}` baked in at build):
- `hms` — `home-manager switch --flake $HOME/code/BrianHicks/dotfiles.nix#<profile>`. **Apply changes.**
- `hmb` — `home-manager build ...`. Build without activating (dry check).
- `hmn` — `home-manager news ...`.
- `hm`  — bare `home-manager`.

Other:
- Format: `nix fmt` (uses `nixfmt-tree`). Run after editing any `.nix`.
- First-time bootstrap (no `home-manager` yet): `scripts/activate-profile.sh (home|work)`.
- CI-equivalent full build of both profiles:
  `nix build --print-build-logs .#homeConfigurations.home.activationPackage .#homeConfigurations.work.activationPackage`
- Build a single profile's activation package: `nix build .#homeConfigurations.<home|work>.activationPackage`

Notes:
- Darwin uses BSD coreutils; prefer the repo's installed GNU tools / `rg` over assuming GNU flags on stock `ls`/`grep`/`sed`.
- There is no test suite — "does it build" via `hmb` / `nix build` is the validation. See `mem:task_completion`.
