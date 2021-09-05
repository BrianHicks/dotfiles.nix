{ pkgs, ... }: {
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import XMonad
      import XMonad.Util.EZConfig

      main = xmonad $ def
          { terminal = "alacritty"
          }
          `additionalKeysP`
            [ ( "M-p", spawn "rofi -show run" )
            ]
    '';
  };
}
