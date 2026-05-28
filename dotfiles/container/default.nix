{ pkgs, ... }:
{
  homebrew.taps = [ "socktainer/tap" ];
  homebrew.formulae = [
    "container"
    "socktainer"
  ];

  home.packages = [
    (pkgs.writeShellScriptBin "container-config-preset" ''
      container system property set build.rosetta true
      container system property set build.cpus 8
      container system property set build.memory 16G
      container system property set container.cpus 8
      container system property set container.memory 8G
    '')
  ];
}
