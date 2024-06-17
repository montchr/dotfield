# TODO: replace with <github:numtide/srvos> profile
{
  config,
  lib,
  modulesPath,
  ...
}:
{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.devices = [ "/dev/sda" ];

  boot.initrd.availableKernelModules = [
    "ahci"
    "xhci_pci"
    "virtio_pci"
    "sd_mod"
    "sr_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;

  swapDevices = [ ];
  hardware.cpu.amd.updateMicrocode = config.hardware.enableRedistributableFirmware;

  fileSystems."/" = {
    # N.B. While nixos-generate-config will set this to a UUID by default, the
    # UUID of a disk on Hetzner Cloud appears to change if the server is
    # rebuilt. We know the root filesystem should always point to this
    # partition, so it's safer to point directly there.
    device = "/dev/sda1";
    fsType = "ext4";
  };

  services.openssh.enable = true;
  services.openssh.openFirewall = true;
  services.openssh.settings.PermitRootLogin = "prohibit-password";
}
