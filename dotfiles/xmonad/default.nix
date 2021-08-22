{ pkgs, ... }: {
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import XMonad
      import XMonad.Util.EZConfig

      main = xmonad $ defaultConfig
          { terminal = "alacritty"
          , modMask = mod4Mask
          }
          `additionalKeysP`
            [ ( "M-p", spawn "rofi" )
            ]
    '';
  };
}
