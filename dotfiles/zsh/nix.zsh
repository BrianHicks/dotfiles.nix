# common nix tasks

function prefetch-github() {
    nix-prefetch-url --unpack https://github.com/$1/$2/archive/$3.tar.gz
}

function prefetch-github-tmpl() {
    echo "pkgs.fetchFromGitHub {"
    echo "  owner = \"$1\";"
    echo "  repo = \"$2\";"
    echo "  rev = \"$3\";"
    echo "  sha256 = \"$(prefetch-github $1 $2 $3)\";"
    echo "};"
}
