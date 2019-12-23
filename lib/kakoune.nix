{ pkgs, lib, ... }:
with lib; {
  mkPlugin = attrs:
    pkgs.stdenv.mkDerivation {
      name = attrs.name;
      src = attrs.src;
      installPhase = ''
        mkdir -p $out
        cp -R . $out/share/kak/${attrs.name}
      '';
    };

  mkPlugins = plugins:
    pkgs.stdenv.mkDerivation {
      name = "kakoune-plugins";
      srcs = ./kakoune;
      buildInputs = plugins ++ [ pkgs.kakoune ];
      buildPhase = ''
        touch autoload

        for derivation in ${concatStrings (intersperse " " plugins)}; do
          for plugin in $(find $derivation/share/kak -mindepth 1 -maxdepth 1); do
            ln -s $plugin autoload/$(basename $plugin)
          done
        done
      '';
      installPhase = ''
        mkdir -p $out/share/kak
        mv autoload $out/share/kak/autoload
      '';
    };
}
