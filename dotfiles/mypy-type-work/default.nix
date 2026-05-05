{ pkgs, ... }:
{
  home.packages = [
    pkgs.mypy-error-count-score
    pkgs.tokei
    (pkgs.writeShellScriptBin "mypy-error-count" "mypy --strict . | wc -l")
  ];
}
