# FIXME: CANNOT BOOT WITHOUT KVM CONSOLE!
# no shell access to zfs decryption prompt in initrd...
{
  config,
  lib,
  pkgs,
  primaryUser,
  ...
}: let
  inherit (builtins) toString;
  inherit (primaryUser) authorizedKeys;

  port = 22;
in {
  boot.initrd = {
    availableKernelModules = [
      "xhci_pci"
      "ahci"
      "nvme"
      "usbhid"
      "uas"
      "sd_mod"
      # Ensure the network adapter is usable during stage 1.
      # $ lspci -v
      "igb"
    ];
    kernelModules = [];
  };

  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    device = "nodev";
  };

  boot.extraModulePackages = [];
}
