{ config, lib, pkgs, ... }:

with lib;

{
  options.homebrew.formulae = mkOption {
    type = types.listOf types.str;
    default = [];
  };

  options.homebrew.casks = mkOption {
    type = types.listOf types.str;
    default = [];
  };

  options.homebrew.taps = mkOption {
    type = types.listOf types.str;
    default = [];
  };

  config = mkIf (!(isNull config.homebrew)) {
    home.activation.installBrews = lib.hm.dag.entryAfter ["writeBoundary"] ''
      $DRY_RUN_CMD ${pkgs.coreutils}/bin/echo "Installing Homebrew packages..."
      $DRY_RUN_CMD ${pkgs.homebrew-sync}/bin/homebrew-sync --taps ${toString config.homebrew.taps} --formulae ${toString config.homebrew.formulae} --casks ${toString config.homebrew.casks}
    '';
  };
}
