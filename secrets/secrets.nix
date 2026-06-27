let
  brian = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFaxA2pk407NVUcLvygdsTLsYIyF9JfQ7jSfT+n06AFs";

  avior = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP95a9BGO3m+97xul/OZ8wuyYEbaCY91V9jj043AQ4hX";
in
{
  "tailscale-key.age".publicKeys = [
    brian
    avior
  ];
}
