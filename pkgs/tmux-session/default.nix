{ sources ? import ../../nix/sources.nix
, pkgs ? import sources.nixpkgs { }
,
}:
pkgs.stdenv.mkDerivation {
  name = "tmux-session";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./tmux-session $out/bin/tmux-session
    wrapProgram $out/bin/tmux-session --prefix PATH : ${
      pkgs.lib.makeBinPath [pkgs.tmux]
    }

    cp ./tmux-jump $out/bin/tmux-jump
    wrapProgram $out/bin/tmux-jump --prefix PATH : ${
      pkgs.lib.makeBinPath [pkgs.findutils pkgs.fzf]
    }
  '';
}
