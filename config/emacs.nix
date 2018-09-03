{ pkgs, ... }:

let
  configs = [
    {
      file = ./emacs/init.el;
      pkgs = epkgs: [ epkgs.use-package epkgs.delight epkgs.general ];
    }
  ];
in
{
  programs.emacs.enable = true;

  programs.emacs.extraPackages = epkgs: builtins.concatLists (builtins.map (config: config.pkgs epkgs) configs);

  home.file.".emacs.d/init.el" = {
    text = builtins.foldl' (soFar: config: soFar + builtins.readFile config.file + "\n") "" configs;
  };
}
