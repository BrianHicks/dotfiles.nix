{ ... }: {
  home.file.".config/nix/nix.conf".text = ''
    substituters = https://cache.nixos.org https://cache.nixos.org/ https://brianhicks-dotfiles.cachix.org
    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= brianhicks-dotfiles.cachix.org-1:TxOMdKYcrai6qqP2IXJcvcPChipzP2GgPfI1DxFasIM=

    builders = ssh://eu.nixbuild.net x86_64-linux - 100 1 big-parallel,benchmark
    builders-use-substitutes = true
  '';
}
