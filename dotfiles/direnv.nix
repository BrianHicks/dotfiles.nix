{ pkgs, ... }:

{
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  # home.file.".direnvrc".text = ''
  #   use_nix() {
  #     eval "$(lorri direnv)"
  #   }
  # '';
}
