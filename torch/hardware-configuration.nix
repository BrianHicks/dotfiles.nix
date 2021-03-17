# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/hardware/network/broadcom-43xx.nix")
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/c6e36cb8-3cfd-470d-aaf3-b3ece1f0289c";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/62C7-5C97";
    fsType = "vfat";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/1dbbf463-6dc4-46a1-8686-1647374bcc84"; }];

  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;
}
