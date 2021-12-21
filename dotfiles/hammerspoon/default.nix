{ pkgs, ... }: {
  home.file.".hammerspoon/init.lua".text = ''
    hyper = {"cmd", "alt", "ctrl", "shift"}

    hs.hotkey.bind(hyper, "F", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = max.h
      win:setFrame(f)
    end)

    hs.loadSpoon("ReloadConfiguration")
    spoon.ReloadConfiguration:start()
  '';

  home.file.".hammerspoon/Spoons".source = pkgs.hammerspoon.spoons;
}
