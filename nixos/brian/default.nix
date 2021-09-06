{ pkgs, ... }: {
  environment.shells = [ pkgs.zsh ];

  users.users.brian = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [
      "wheel" # Enable ‘sudo’ for the user.
      "audio"
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.brian = import ../../dotfiles;
  };
}
