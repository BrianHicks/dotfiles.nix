{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "dotfiles";
  buildInputs = [
    ## meta-information for managing dotfiles
    niv.niv

    ## formatting
    python3Packages.black

    ## sample projects
    # elm
    # elmPackages.elm
    # elmPackages.elm-format
  ];
}
