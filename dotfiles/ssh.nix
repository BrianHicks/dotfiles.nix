{ pkgs, ... }:

{
  programs.ssh = {
    enable = true;

    extraConfig = "Include config.d/*";
  };
}
