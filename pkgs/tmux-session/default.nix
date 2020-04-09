{ sources ? import ../../nix/sources.nix, nixpkgs ? import sources.nixpkgs { }
}:
with nixpkgs;
stdenv.mkDerivation {
  name = "tmux-session";

  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    cp ./tmux-session $out/bin/tmux-session
  '';
}
