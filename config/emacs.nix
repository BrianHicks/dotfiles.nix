{ pkgs, ... }:

{
  programs.emacs.enable = true;
  programs.emacs.extraPackages = epkgs: [
    epkgs.evil
  ];

  # home.file.".emacs.d" = {
  #   source = ./emacs;
  # };
}
