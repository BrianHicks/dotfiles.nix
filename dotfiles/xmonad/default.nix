{ pkgs, ... }: {
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import XMonad

      main = xmonad defaultConfig
          { terminal = "${pkgs.alacritty}/bin/alacritty"
          , modMask = mod4Mask
          }
    '';
  };
}
