HERE="$(realpath $(dirname $0))"
make
darwin-rebuild $@ -I "darwin-config=$HERE/darwin/default.nix"
