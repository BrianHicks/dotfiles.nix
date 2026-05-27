{ pkgs, ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  programs.peon-ping.claudeCodeIntegration = true;

  programs.claude-code = {
    enable = true;
    enableMcpIntegration = true;

    commandsDir = ./commands;
    skills = ../robot-friends/skills;

    plugins = [
      "${pkgs.crit.src}/integrations/claude-code"
      "${pkgs.learning-opportunities}/learning-opportunities"
      "${pkgs.learning-opportunities}/learning-opportunities-auto"
    ];
  };

}
