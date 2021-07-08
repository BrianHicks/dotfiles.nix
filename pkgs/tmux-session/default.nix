{ sources ? import ../../nix/sources.nix, pkgs ? import sources.nixpkgs { }
, tmux ? pkgs.callPackage ../tmux { } }:
pkgs.stdenv.mkDerivation {
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
      pkgs.lib.makeBinPath [ pkgs.findutils pkgs.fzf ]
    }
  '';
}
