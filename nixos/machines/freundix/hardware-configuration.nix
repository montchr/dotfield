# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{
  lib,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "virtio_pci" "usbhid" "usb_storage" "sr_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = [];
  boot.extraModulePackages = [];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/11848a3d-e365-4a43-9884-565db22fe6a8";
    fsType = "btrfs";
    options = [
      "subvol=@root"
      "defaults"
      "x-mount.mkdir"
      "noatime"
      "compress=zstd"
    ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/11848a3d-e365-4a43-9884-565db22fe6a8";
    fsType = "btrfs";
    options = [
      "subvol=@store"
      "defaults"
      "x-mount.mkdir"
      "noatime"
      "compress=zstd"
    ];
  };

  fileSystems."/var/log" = {
    device = "/dev/disk/by-uuid/11848a3d-e365-4a43-9884-565db22fe6a8";
    fsType = "btrfs";
    options = [
      "subvol=@log"
      "defaults"
      "x-mount.mkdir"
      "noatime"
      "compress=zstd"
    ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/11848a3d-e365-4a43-9884-565db22fe6a8";
    fsType = "btrfs";
    options = [
      "subvol=@home"
      "defaults"
      "x-mount.mkdir"
      "noatime"
      "compress=zstd"
    ];
  };

  fileSystems."/persist" = {
    device = "/dev/disk/by-uuid/11848a3d-e365-4a43-9884-565db22fe6a8";
    fsType = "btrfs";
    options = [
      "subvol=@persist"
      "defaults"
      "x-mount.mkdir"
      "noatime"
      "compress=zstd"
    ];
  };

  fileSystems."/var/lib/postgres" = {
    device = "/dev/disk/by-uuid/11848a3d-e365-4a43-9884-565db22fe6a8";
    fsType = "btrfs";
    options = [
      "subvol=@postgres"
      "defaults"
      "x-mount.mkdir"
      "noatime"
      "compress=zstd"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/3927-6277";
    fsType = "vfat";
  };

  swapDevices = [];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp0s1.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
}
