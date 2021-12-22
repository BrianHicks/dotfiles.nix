{ pkgs, ... }: {
  home.file.".hammerspoon/init.lua".text = ''
    hyper = {"cmd", "alt", "ctrl", "shift"}

    hs.hotkey.bind(hyper, "F", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = hs.mouse.getCurrentScreen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = max.h

      win:moveToScreen(screen)
      win:setFrame(f)
    end)

    hs.hotkey.bind(hyper, "M", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = hs.mouse.getCurrentScreen()
      local max = screen:frame()

      local segments = 6
      local segmentW = max.w / segments
      local segmentH = max.h / segments

      f.x = max.x + segmentW
      f.y = max.y + segmentH
      f.w = segmentW * 4
      f.h = segmentH * 4

      win:moveToScreen(screen)
      win:setFrame(f)
    end)

    hs.hotkey.bind(hyper, "Z", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = hs.mouse.getCurrentScreen()
      local max = screen:frame()

      local segments = 6
      local segmentW = max.w / segments
      local segmentH = max.h / segments

      f.x = max.x + segmentW
      f.y = max.y
      f.w = segmentW * 4
      f.h = segmentH * 5

      win:moveToScreen(screen)
      win:setFrame(f)
    end)

    hs.hotkey.bind(hyper, "S", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = hs.mouse.getCurrentScreen()
      local max = screen:frame()

      local segments = 6
      local segmentH = max.h / segments

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = segmentH * 4

      win:moveToScreen(screen)
      win:setFrame(f)
    end)

    hs.hotkey.bind(hyper, "T", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = hs.mouse.getCurrentScreen()
      local max = screen:frame()

      local segments = 6
      local segmentH = max.h / segments

      f.x = max.x
      f.y = max.y + segmentH * 4
      f.w = max.w
      f.h = segmentH * 2

      win:moveToScreen(screen)
      win:setFrame(f)
    end)

    hs.hotkey.bind(hyper, "Left", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = hs.mouse.getCurrentScreen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h

      win:moveToScreen(screen)
      win:setFrame(f)
    end)

    hs.hotkey.bind(hyper, "Right", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = hs.mouse.getCurrentScreen()
      local max = screen:frame()

      f.x = max.x + max.w / 2
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h

      win:moveToScreen(screen)
      win:setFrame(f)
    end)

    hs.hotkey.bind(hyper, "Up", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = hs.mouse.getCurrentScreen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = max.h / 2

      win:moveToScreen(screen)
      win:setFrame(f)
    end)

    hs.hotkey.bind(hyper, "Down", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = hs.mouse.getCurrentScreen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y + max.h / 2
      f.w = max.w
      f.h = max.h / 2

      win:moveToScreen(screen)
      win:setFrame(f)
    end)

    hs.hotkey.bind(hyper, "C", function()
      local win = hs.window.focusedWindow()
      local screen = hs.mouse.getCurrentScreen()
      win:centerOnScreen(screen)
    end)

    hs.loadSpoon("ReloadConfiguration")
    spoon.ReloadConfiguration:start()
  '';

  home.file.".hammerspoon/Spoons".source = pkgs.hammerspoon.spoons;
}
