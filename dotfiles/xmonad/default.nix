{ pkgs, ... }: {
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import XMonad
      import XMonad.Util.EZConfig

      main = xmonad $ def
          { terminal = "${pkgs.alacritty}/bin/alacritty"
          , modMask = mod4Mask
          , startupHook = spawn "xset r rate 300 50"
          }
          `additionalKeysP`
            [ ( "M-p", spawn "rofi -show run" )

              -- reminder: look up key presses with xev

              -- volume
            , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 5%+" )
            , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 5%-" )
            , ("<XF86AudioMute>", spawn "amixer sset Master toggle" )
            ]
    '';
  };
}
