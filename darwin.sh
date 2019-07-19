HERE="$(realpath $(dirname $0))"
darwin-rebuild $@ -I "darwin-config=$HERE/macbook-darwin.nix"
