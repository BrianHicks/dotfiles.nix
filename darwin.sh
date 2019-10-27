HERE="$(dirname $0)"
darwin-rebuild $@ -I "darwin-config=$HERE/darwin/default.nix"
