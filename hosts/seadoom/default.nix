# FIXME: importables not available yet
# FIXME: is modulesPath available?

{ profiles, suites, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    profiles.system.linode
  ] ++ suites.nixos;

  # TODO: remove or something
  # bud.enable = true;
  # bud.localFlakeClone = "/etc/nixos";

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  boot.initrd.availableKernelModules = [ "virtio_pci" "virtio_scsi" "ahci" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/sda";
    fsType = "ext4";
  };

  swapDevices = [
    { device = "/dev/sdb"; }
  ];

  time.timeZone = "America/New_York";

  services.openssh.permitRootLogin = "no";
  services.openssh.openFirewall = true;

  system.stateVersion = "21.05";
}
