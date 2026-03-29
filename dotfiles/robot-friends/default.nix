{ pkgs, ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  homebrew = {
    taps = [ "anomalyco/tap" ];
    formulae = [
      "opencode-desktop"
      "lm-studio"
    ];
  };

  home.file.".claude/commands".source = ./commands;

  programs.claude-code = {
    enable = true;
  };

  programs.gemini-cli = {
    enable = true;
  };

  home.packages = [ pkgs.ollama ];
}
