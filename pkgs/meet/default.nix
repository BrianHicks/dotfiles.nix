{ sources ? import ../../nix/sources.nix
, pkgs ? import sources.nixpkgs { }
,
}:
pkgs.stdenv.mkDerivation {
  name = "meet";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./meet.py $out/bin/meet
    wrapProgram $out/bin/meet \
      --set MEET_CALENDARS "brian@noredink.com,brian@brianthicks.com" \
      --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.python3 pkgs.montage]}
  '';
}
