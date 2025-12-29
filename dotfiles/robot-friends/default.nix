{ ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  programs.claude-code = {
    enable = true;
  };

  programs.gemini-cli = {
    enable = true;
  };
}
