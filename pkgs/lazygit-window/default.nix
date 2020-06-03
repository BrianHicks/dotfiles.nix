{ sources ? import ../../nix/sources.nix, nixpkgs ? import sources.nixpkgs { }
}:
with nixpkgs;
stdenv.mkDerivation {
  name = "lazygit-window";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./lazygit-window.sh $out/bin/lazygit-window
    wrapProgram $out/bin/lazygit-window --prefix PATH : ${
      pkgs.lib.makeBinPath [ tmux lazygit ]
    }
  '';
}

