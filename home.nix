{
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
            ./dotfiles/anki
            ./dotfiles/aws
            ./dotfiles/backrest
            ./dotfiles/bambu-studio
            ./dotfiles/gh
            ./dotfiles/signal
            ./dotfiles/steam
          ]
        else if specialArgs.profile == "work" then
          [ ]
        else
          [ ];
      commonImports = [
        ./dotfiles/1password
        ./dotfiles/aerospace
        ./dotfiles/chrome
        ./dotfiles/diffoscope
        ./dotfiles/docker-desktop
        ./dotfiles/discord
        ./dotfiles/dropbox
        ./dotfiles/element
        ./dotfiles/firefox
        ./dotfiles/fzf
        ./dotfiles/ghostty
        ./dotfiles/git
        ./dotfiles/glab
        ./dotfiles/gpg
        ./dotfiles/jj
        ./dotfiles/json
        ./dotfiles/k9s
        ./dotfiles/lazygit
        ./dotfiles/lf
        ./dotfiles/mise
        ./dotfiles/nix
        ./dotfiles/obsidian
        ./dotfiles/python
        ./dotfiles/raycast
        ./dotfiles/ripgrep
        ./dotfiles/robot-friends
        ./dotfiles/slack
        ./dotfiles/tidal
        ./dotfiles/tofu
        ./dotfiles/tree
        ./dotfiles/zed
        ./dotfiles/zoom
        ./dotfiles/zsh
      ];
    in
    commonImports ++ profileImports;

  home.shellAliases = {
    # Home-manager commands
    hm = "home-manager";
    hms = "home-manager switch --flake $HOME/code/BrianHicks/dotfiles.nix#${specialArgs.profile}";
    hmb = "home-manager build --flake $HOME/code/BrianHicks/dotfiles.nix#${specialArgs.profile}";
    hmn = "home-manager news --flake $HOME/code/BrianHicks/dotfiles.nix#${specialArgs.profile}";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
