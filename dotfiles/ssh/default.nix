{ pkgs, ... }: {
  programs.ssh = {
    enable = true;
    extraConfig = ''
      AddKeysToAgent yes
      Include config.d/*
      VisualHostKey yes
    '';
  };
}
