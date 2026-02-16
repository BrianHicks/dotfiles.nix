{ ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  homebrew = {
    taps = [ "anomalyco/tap" ];
    formulae = [ "opencode-desktop" ];
  };

  programs.claude-code = {
    enable = true;
  };

  programs.gemini-cli = {
    enable = true;
  };
}
