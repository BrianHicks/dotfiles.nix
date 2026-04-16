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
    tools = ./opencode/tools;
  };

  services.ollama.enable = true;

  home.packages = [ pkgs.openspec ];
}
