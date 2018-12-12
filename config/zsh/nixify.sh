# copied from https://github.com/direnv/direnv/wiki/Nix
latest_shell() {
  LATEST_RELEASE="$(curl -Ss --head https://nixos.org/channels/nixpkgs-18.09-darwin | grep -i Location | sed -e 's/\r//g' | awk '{ print $2 }')"
  GIT_REVISION="$(curl -sS "$LATEST_RELEASE/git-revision")"
  GITHUB_URL="https://github.com/nixos/nixpkgs/archive/${GIT_REVISION}.tar.gz"
  GITHUB_HASH="$(nix-prefetch-url --unpack "$GITHUB_URL")"

  echo 'with import (builtins.fetchTarball rec {'
  echo '  # grab a hash from here: https://nixos.org/channels/'
  echo "  name = \"$(basename "$LATEST_RELEASE")\";";
  echo "  url = \"$GITHUB_URL\";"
  echo '  # Hash obtained using `nix-prefetch-url --unpack <url>`'
  echo "  sha256 = \"$GITHUB_HASH\";"
  echo '}) {};'
  echo ''
  echo 'stdenv.mkDerivation {'
  echo "  name = \"$(basename $(pwd))\";"
  echo '  buildInputs = ['
  echo '    git'
  echo '  ];'
  echo '}'
}

nixify() {
  if [[ ! -e ./.envrc ]]; then
    echo "use nix" > .envrc
    direnv allow
  fi

  if [[ ! -e shell.nix ]]; then
    latest_shell > shell.nix
    ${EDITOR:-vim} shell.nix
  fi
}
