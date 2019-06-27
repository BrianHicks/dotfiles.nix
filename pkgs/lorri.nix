{ pkgs }:

let
  src =
    pkgs.fetchFromGitHub {
      owner = "target";
      repo = "lorri";
      rev = "80ca3e7c12f74af035cdeff289ba2aa3c8950cb2";
      sha256 = "05a0nrg9hp4li5nmyf4a5975p4amq19f17rqxncf7pcagyw0sax2";
    };
in
  pkgs.callPackage src { inherit src; }
