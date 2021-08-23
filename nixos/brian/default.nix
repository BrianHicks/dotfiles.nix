{ pkgs, ... }: {
  users.users.brian = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.brian = import ../../dotfiles;
  };
}
