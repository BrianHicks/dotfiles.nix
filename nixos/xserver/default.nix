{ pkgs, ... }: {
  services.xserver = {
    layout = "us";

    libinput = {
      enable = true;

      naturalScrolling = true;
      touchpad.disableWhileTyping = true;
    };
  };
}
