{ config, pkgs, ... }:
let sources = import ../nix/sources.nix { };
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../home-manager/nixos
    ("${sources.nixos-hardware}/apple/macbook-pro/12-1")
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "torch"; # Define your hostname.
  networking.wireless.enable =
    false; # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.interfaces.wlp3s0.useDHCP = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME 3 Desktop Environment.
  services.xserver.displayManager.gdm = {
    enable = true;
    wayland = true;
  };
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  # Audio
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull; # JACK, bluetooth
  };
  environment.systemPackages = [ pkgs.pulseaudio-ctl ];

  # Brightness
  hardware.brillo.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.brian = {
    isNormalUser = true;
    shell = pkgs.zsh;

    # Enable ‘sudo’ for the user.
    extraGroups = [ "wheel" "networkmanager" "video" ];
  };

  home-manager.users.brian = { imports = [ ../dotfiles ./sway.nix ]; };

  systemd.user.services.ssh-agent = {
    enable = true;
    wantedBy = [ "multi-user.target" ];

    description = "SSH key agent";
    documentation = [
      "https://wiki.archlinux.org/index.php/SSH_keys#Start_ssh-agent_with_systemd_user"
    ];

    environment = {
      SSH_AUTH_SOCK = "%t/ssh-agent.socket";
      DISPLAY = ":0";
    };
    script = "${pkgs.openssh}/bin/ssh-agent -D -a $SSH_AUTH_SOCK";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

