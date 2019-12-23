{ pkgs, lib, ... }:
with lib; {
  mkPlugin = attrs:
    pkgs.stdenv.mkDerivation {
      name = attrs.name;
      src = attrs.src;
      installPhase = ''
        mkdir -p $out/share/kak/autoload
        cp -R . $out/share/kak/autoload/${attrs.name}
      '';
    };

  mkPlugins = plugins:
    pkgs.stdenv.mkDerivation {
      name = "kakoune-plugins";
      srcs = ./kakoune;
      buildInputs = plugins;
      buildPhase = ''
        mkdir autoload

        for derivation in ${concatStrings (intersperse " " plugins)}; do
          for plugin in $(find $derivation/share/kak/autoload -mindepth 1 -maxdepth 1); do
            ln -s $plugin autoload/$(basename $plugin)
          done
        done
      '';
      installPhase = ''
        mkdir -p $out/share/kak
        mv autoload $out/share/kak/autoload
      '';
    };

  mkColorPlugin = attrs:
    pkgs.stdenv.mkDerivation {
      name = attrs.name;
      src = attrs.src;
      installPhase = ''
        mkdir -p $out/share/kak/colors
        cp -R . $out/share/kak/colors/${attrs.name}
      '';
    };

  mkColors = plugins:
    pkgs.stdenv.mkDerivation {
      name = "kakoune-plugins";
      srcs = ./kakoune;
      buildInputs = plugins;
      buildPhase = ''
        mkdir colors

        for derivation in ${concatStrings (intersperse " " plugins)}; do
          for plugin in $(find $derivation/share/kak/colors -mindepth 1 -maxdepth 1); do
            ln -s $plugin colors/$(basename $plugin)
          done
        done
      '';
      installPhase = ''
        mkdir -p $out/share/kak
        mv colors $out/share/kak/colors
      '';
    };
}
