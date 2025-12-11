{ pkgs, ... }:
{
  # Note: need to put `trusted-users = brianhicks` in `/etc/nix/nix.conf` for this to work.
  home.file.".config/nix/nix.conf".text = ''
    substituters = https://cache.nixos.org https://cache.nixos.org/ https://brianhicks-dotfiles.cachix.org https://bytes-zone.cachix.org
    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= brianhicks-dotfiles.cachix.org-1:TxOMdKYcrai6qqP2IXJcvcPChipzP2GgPfI1DxFasIM= bytes-zone.cachix.org-1:9ktbFm0FsAS6Qo3zSS69fbQRf44Wtk6yQYdH1Hiioi8=

    builders = ssh://eu.nixbuild.net x86_64-linux - 100 1 big-parallel,benchmark
    builders-use-substitutes = true
  '';

  home.packages = [
    pkgs.nil
    pkgs.nixd
  ];
}
