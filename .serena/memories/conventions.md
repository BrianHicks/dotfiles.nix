# Conventions

## Per-tool modules
- One directory per tool: `dotfiles/<tool>/default.nix`, a Home Manager module.
- Standard signature `{ config, pkgs, ... }:` (add `specialArgs` only if branching on profile). Body sets `programs.<tool>` / `home.*` options. Prefer upstream HM `programs.*` options over hand-rolled `home.file` dotfiles when available.
- Cross-tool references go through config, not literals: e.g. `config.programs.git.settings.user.name`, and binaries via `${pkgs.difftastic}/bin/difft`.
- **To enable a tool**: add `./dotfiles/<tool>` to the import list in `home.nix` — `commonImports` (all profiles) or the `home`/`work` branch of `profileImports`. Creating the dir alone does nothing.

## Custom packages
- `pkgs/<name>/default.nix`, pattern `{ pkgs ? import <nixpkgs> {} }: pkgs.stdenv.mkDerivation { ... }`.
- Scripts wrapped with `makeWrapper` / `wrapProgram --prefix PATH` to inject runtime deps (see `pkgs/git-gclone`).
- **To expose a package**: add a `callPackage ./pkgs/<name> {}` line to the overlay in `flake.nix`. Then it is available as `pkgs.<name>` everywhere.

## Style
- Formatted by `nixfmt-tree` — the one-item-per-line list layout is its output, not hand styling. Always `nix fmt` after edits; don't fight the formatter.
- Comments are sparse and explain *why* (e.g. non-obvious nix.conf / builder requirements), not *what*.
