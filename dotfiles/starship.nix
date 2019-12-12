{ pkgs, ... }:

{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      add_newline = true;
      prompt_order = [
        "kubernetes"
        "directory"
        "git_branch"
        "git_commit"
        "git_state"
        "git_status"
        "package"
        "nix_shell"
        "aws"
        "env_var"
        "cmd_duration"
        "jobs"
        "battery"
        "time"
        "character"
      ];

      git_branch.symbol = "";
      git_status = {
        style = "bold green";
        prefix = "";
        suffix = " ";
      };
    };
  };
}
