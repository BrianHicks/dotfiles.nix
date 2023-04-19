{ pkgs, ... }: {
  home.file.".hammerspoon/init.lua".text =
    # lua
    ''
      hyper = {"cmd", "alt", "ctrl", "shift"}

      function resizeToCursorScreen(modifyDimensions)
        local win = hs.window.focusedWindow()
        local windowFrame = win:frame()
        local screen = hs.mouse.getCurrentScreen()
        local screenFrame = screen:frame()

        modifyDimensions(windowFrame, screenFrame)

        win:moveToScreen(screen)
        win:setFrame(windowFrame)
      end

      function resizeToCursorScreenOnGrid(gridColumns, gridRows, xOffset, yOffset, width, height)
        resizeToCursorScreen(function(windowFrame, screenFrame)
          local segmentsW = screenFrame.w / gridColumns
          local segmentsH = screenFrame.h / gridRows

          windowFrame.x = screenFrame.x + xOffset * segmentsW
          windowFrame.y = screenFrame.y + yOffset * segmentsH
          windowFrame.w = segmentsW * width
          windowFrame.h = segmentsH * height
        end)
      end

      hs.hotkey.bind(hyper, "F", function()
        resizeToCursorScreen(function(windowFrame, screenFrame)
          windowFrame.x = screenFrame.x
          windowFrame.y = screenFrame.y
          windowFrame.w = screenFrame.w
          windowFrame.h = screenFrame.h
        end)
      end)

      hs.hotkey.bind(hyper, "M", function()
        resizeToCursorScreenOnGrid(6, 6, 1, 1, 4, 4)
      end)

      hs.hotkey.bind(hyper, "Z", function()
        resizeToCursorScreenOnGrid(6, 6, 1, 0, 4, 5)
      end)

      hs.hotkey.bind(hyper, "S", function()
        resizeToCursorScreenOnGrid(6, 6, 0, 0, 6, 4)
      end)

      hs.hotkey.bind(hyper, "T", function()
        resizeToCursorScreenOnGrid(6, 6, 0, 4, 6, 2)
      end)

      hs.hotkey.bind(hyper, "Left", function()
        resizeToCursorScreenOnGrid(2, 1, 0, 0, 1, 1)
      end)

      hs.hotkey.bind(hyper, "Right", function()
        resizeToCursorScreenOnGrid(2, 1, 1, 0, 1, 1)
      end)

      hs.hotkey.bind(hyper, "Up", function()
        resizeToCursorScreenOnGrid(1, 2, 0, 0, 1, 1)
      end)

      hs.hotkey.bind(hyper, "Down", function()
        resizeToCursorScreenOnGrid(1, 2, 0, 1, 1, 1)
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
