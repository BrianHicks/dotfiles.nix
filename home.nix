{
  config,
  pkgs,
  specialArgs,
  ...
}:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "brianhicks";
  home.homeDirectory = "/Users/brianhicks";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.11"; # Please read the comment before changing.

  imports =
    let
      profileImports =
        if specialArgs.profile == "home" then
          [
            ./dotfiles/backrest
            ./dotfiles/bambu-studio
            ./dotfiles/signal
          ]
        else if specialArgs.profile == "work" then
          [ ./dotfiles/k9s ]
        else
          [ ];
      commonImports = [
        ./dotfiles/1password
        ./dotfiles/aerospace
        ./dotfiles/dropbox
        ./dotfiles/firefox
        ./dotfiles/fzf
        ./dotfiles/ghostty
        ./dotfiles/git
        ./dotfiles/gpg
        ./dotfiles/json
        ./dotfiles/lazygit
        ./dotfiles/lf
        ./dotfiles/nixconfig
        ./dotfiles/obsidian
        ./dotfiles/raycast
        ./dotfiles/slack
        ./dotfiles/zed
        ./dotfiles/zsh
      ];
    in
    commonImports ++ profileImports;

  home.shellAliases = {
    # Home-manager commands
    hm = "home-manager";
    hms = "home-manager switch --flake $HOME/code/BrianHicks/dotfiles.nix#${specialArgs.profile}";
    hmb = "home-manager build --flake $HOME/code/BrianHicks/dotfiles.nix#${specialArgs.profile}";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
