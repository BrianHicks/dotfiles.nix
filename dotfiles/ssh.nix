{ pkgs, ... }:

{
  programs.ssh = {
    enable = true;

    controlMaster = "auto";
    controlPath = "~/.ssh/control/%r@%h:%p";
    controlPersist = "5m";

    extraConfig = "Include config.d/*";
  };
}
