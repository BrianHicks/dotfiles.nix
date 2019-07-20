{ pkgs }:

let
  src =
    pkgs.fetchFromGitHub {
      owner = "target";
      repo = "lorri";
      rev = "d3e452ebc2b24ab86aec18af44c8217b2e469b2a";
      sha256 = "07yf3gl9sixh7acxayq4q8h7z4q8a66412z0r49sr69yxb7b4q89";
    };
in
  pkgs.callPackage src { inherit src; }
