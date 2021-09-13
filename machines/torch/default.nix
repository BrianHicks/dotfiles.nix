# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
inputs:
{ config, pkgs, ... }:

{
  imports = [
    # hardware
    ./hardware-configuration.nix
    "${inputs.nixos-hardware}/apple/macbook-pro/12-1"

    # system setup
    (import ../../nixos/nix inputs)

    ../../nixos/1password
    ../../nixos/bluetooth
    ../../nixos/brian
    ../../nixos/i3
    ../../nixos/gnome-keyring
    ../../nixos/networkmanager
    ../../nixos/pulseaudio
    ../../nixos/redshift
    ../../nixos/upower
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "torch"; # Define your hostname.
  networking.wireless.enable =
    false; # Enables wireless support via wpa_supplicant.
  # Display settings
  # this is a Retina display at 227 DPI, but it's way too big!
  services.xserver.dpi = 163;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp3s0.useDHCP = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
