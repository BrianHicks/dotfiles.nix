{ pkgs ? import <nixpkgs> { }, ... }:
pkgs.stdenv.mkDerivation {
  name = "chromium-wayland";

  src = ./.;

  buildInputs = with pkgs; [ makeWrapper chromium ];
  buildPhase = "true";

  installPhase = ''
    mkdir $out/

    ln -s ${pkgs.chromium}/bin $out/bin

    mkdir $out/share
    ln -s ${pkgs.chromium}/share/icons $out/share/icons
    ln -s ${pkgs.chromium}/share/man $out/share/man

    # the rest of this setup is all to prepare with this... just adding two
    # flags to the desktop entry!
    mkdir $out/share/applications
    sed 's/Exec=chromium/\0 --enable-features=UseOzonePlatform --ozone-platform=wayland/g' \
        ${pkgs.chromium}/share/applications/chromium-browser.desktop \
        > $out/share/applications/chromium-browser.desktop
  '';
}
