{
  pkgs ? import <nixpkgs> { },
}:
pkgs.buildGoModule rec {
  pname = "ncr";
  version = "0.2.0";

  src = pkgs.fetchFromGitHub {
    owner = "justinabrahms";
    repo = "ncr";
    rev = "v${version}";
    hash = "sha256-FI1PQWDnJnEkyumNoss9Ux/ecMP8hqP68ujXWzVkNUU=";
  };

  vendorHash = "sha256-Sjh4hR5PBt3zmmhA5nEtcs5+HDd2V5Zwbq739dbHPW8=";

  meta = with pkgs.lib; {
    description = "Narrative Code Review: turn a GitHub PR into a story you read outside-in";
    homepage = "https://github.com/justinabrahms/ncr";
    license = licenses.mit;
    mainProgram = "ncr";
  };
}
