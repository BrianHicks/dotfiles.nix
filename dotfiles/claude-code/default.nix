{ pkgs, ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  programs.claude-code = {
    enable = true;
    enableMcpIntegration = true;

    commandsDir = ./commands;

    plugins = [ "${pkgs.crit.src}/integrations/claude-code" ];
  };

}
