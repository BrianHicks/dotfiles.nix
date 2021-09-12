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
            , ("<XF86MonBrightnessUp>", spawn "${pkgs.brightnessctl}/bin/brightnessctl -d intel_backlight set 5%+")
            , ("<XF86MonBrightnessDown>", spawn "${pkgs.brightnessctl}/bin/brightnessctl -d intel_backlight set 5%-")
            , ("<XF86KbdBrightnessUp>", spawn "${pkgs.brightnessctl}/bin/brightnessctl -d smc::kbd_backlight set 5%+")
            , ("<XF86KbdBrightnessDown>", spawn "${pkgs.brightnessctl}/bin/brightnessctl -d smc::kbd_backlight set 5%-")
            ]
    '';
  };
}
