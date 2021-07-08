{ sources ? import ../../nix/sources.nix, pkgs ? import sources.nixpkgs { }
, tmux ? pkgs.callPackage ../tmux { } }:
pkgs.stdenv.mkDerivation {
  # inspired by one of Stöffel's keybindings at
  # https://github.com/stoeffel/.dots/blob/master/tmux/keybindings.conf
  name = "lazygit-window";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./lazygit-window.sh $out/bin/lazygit-window
    wrapProgram $out/bin/lazygit-window --prefix PATH : ${
      pkgs.lib.makeBinPath [ tmux pkgs.lazygit ]
    }
  '';
}

