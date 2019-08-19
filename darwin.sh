HERE="$(realpath $(dirname $0))"
make dotfiles/neovim/plugins.nix
darwin-rebuild $@ -I "darwin-config=$HERE/darwin/default.nix"
