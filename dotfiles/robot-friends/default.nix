{ pkgs, ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  homebrew = {
    taps = [ "anomalyco/tap" ];
    formulae = [ "opencode" ];
  };

  programs.claude-code = {
    enable = true;
    commandsDir = ./commands;
  };

  programs.gemini-cli = {
    enable = true;
  };

  home.packages = [ pkgs.ollama ];
}
