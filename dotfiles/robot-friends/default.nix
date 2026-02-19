{ ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  homebrew = {
    taps = [ "anomalyco/tap" ];
    formulae = [ "opencode-desktop" ];
  };

  home.file.".claude/commands".source = ./commands;

  programs.claude-code = {
    enable = true;
  };

  programs.gemini-cli = {
    enable = true;
  };
}
