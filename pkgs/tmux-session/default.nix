{ sources ? import ../../nix/sources.nix, nixpkgs ? import sources.nixpkgs { }
}:
with nixpkgs;
stdenv.mkDerivation {
  name = "tmux-session";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./tmux-session $out/bin/tmux-session
    wrapProgram $out/bin/tmux-session --prefix PATH : ${
      pkgs.lib.makeBinPath [ tmux ]
    }

    cp ./tmux-jump $out/bin/tmux-jump
    wrapProgram $out/bin/tmux-jump --prefix PATH : ${
      pkgs.lib.makeBinPath [ findutils fzf ]
    }
  '';
}
