{ pkgs, ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  programs.claude-code = {
    enable = true;
    commandsDir = ./commands;
  };

  programs.opencode = {
    enable = true;
    agents = ./opencode/agents;
    commands = ./opencode/commands;
  };

  home.packages = [ pkgs.ollama ];
}
