inputs:
{ pkgs, ... }: {
  nix = {
    # TODO: might have to disable this to get home-manager working
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs = {
    config.allowUnfree = true;
  };
}
