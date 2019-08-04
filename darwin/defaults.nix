{ ... }:

{
  system.keyboard = {
    enableKeyMapping = true;

    # use caps lock as ctrl instead of YELLING
    remapCapsLockToControl = true;
  };

  system.defaults = {
    NSGlobalDomain = {
      # expand the save panel by default
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;

      # Disable automatic typography options I find annoying while typing code
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;

      # enable tap-to-click (mode 1)
      "com.apple.mouse.tapBehavior" = 1;

      # Enable full keyboard access for all controls
      # (e.g. enable Tab in modal dialogs)
      AppleKeyboardUIMode = 3;

      # Disable press-and-hold for keys in favor of key repeat
      ApplePressAndHoldEnabled = false;

      # Set a very fast keyboard repeat rate
      KeyRepeat = 2;
      InitialKeyRepeat = 10;

      # Enable subpixel font rendering on non-Apple LCDs
      # Reference: https://github.com/kevinSuttle/macOS-Defaults/issues/17#issuecomment-266633501
      AppleFontSmoothing = 1;

      # Finder: show all filename extensions
      AppleShowAllExtensions = true;
    };

    finder = {
      # show full POSIX path as Finder window title
      _FXShowPosixPathInTitle = true;

      # disable the warning when changing a file extension
      FXEnableExtensionChangeWarning = false;
    };

    trackpad = {
      # drag with three fingers
      TrackpadThreeFingerDrag = true;
    };

    dock = {
      # set the icon size of all dock items
      tilesize = 40;

      # enable spring loading (hold a dragged file over an icon to drop/open it there)
      enable-spring-load-actions-on-all-items = true;

      # show indicator lights for open applications
      show-process-indicators = true;

      # don't automatically rearrange spaces based on the most recent one
      mru-spaces = false;

      # show hidden applications as translucent
      showhidden = true;
    };

    screencapture.location = "/Users/brianhicks/Downloads";

    LaunchServices = {
      # Disable the "Are you sure you want to open this application?" dialog
      LSQuarantine = false;
    };
  };
}
