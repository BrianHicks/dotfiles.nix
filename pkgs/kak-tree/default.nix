{ pkgs
, naersk
, ...
}:
let
  src = pkgs.fetchgit {
    url = "https://github.com/ul/kak-tree.git";
    rev = "8c0b6b4a3d5750732817aa8c9e633699cb5c2367";
    sha256 = "1xd5qa8im0rjplp23v2fymh80kp1z25r7fd1v65s5wndxd49c0cs";
    fetchSubmodules = true;
  };
in
{
  kakounePlugins.kak-tree = pkgs.kakouneUtils.buildKakounePlugin {
    pname = "kak-tree";
    version = src.rev;
    src = src;
  };

  kak-tree = naersk.buildPackage {
    root = src;
    cargoBuildOptions = defaults:
      defaults
      ++ [
        ''--features "bash css elm haskell html javascript json python ruby"''
      ];
  };
}
