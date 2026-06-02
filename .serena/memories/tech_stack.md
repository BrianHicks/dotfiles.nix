# Tech Stack

- **Nix flakes** + **Home Manager** (`github:nix-community/home-manager`, nixpkgs follows).
- **nixpkgs**: `nixpkgs-unstable` channel.
- `home.stateVersion = "25.11"` — do not bump casually (see HM release notes first).
- Bootstrap pin: `scripts/activate-profile.sh` runs `home-manager/release-25.11` (release branch), distinct from the unstable nixpkgs used by the flake itself.
- Target system: `aarch64-darwin` (Apple Silicon macOS).
- Formatter: **`nixfmt-tree`** (exposed as the flake `formatter`; run via `nix fmt`).
- Nix LSPs available in env (via `dotfiles/nix`): `nil` and `nixd` — Serena's nix LSP works here.
- Flake inputs beyond nixpkgs/home-manager: `flake-utils`, `crit`, `peon-ping` (provides a homeManagerModule), `serena`, `learning-opportunities` (`flake = false`, source-only).
- Languages present in custom code: bash (`pkgs/git-*`), python (`pkgs/homebrew-sync`, `list-python-tests`, `mypy-error-count-score`), some TS/JS under `dotfiles/robot-friends/opencode` and `scripts/`.
- Binary cache: cachix `brianhicks-dotfiles` (+ `bytes-zone`); remote builders via `nixbuild.net` (configured in `dotfiles/nix/default.nix`).
