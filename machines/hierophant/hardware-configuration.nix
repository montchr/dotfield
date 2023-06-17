# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/profiles/qemu-guest.nix")
    ];

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "virtio_scsi" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/ebb73594-4d66-42df-afa2-36417c1069ce";
      fsType = "btrfs";
      options = [ "subvol=@root" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/ebb73594-4d66-42df-afa2-36417c1069ce";
      fsType = "btrfs";
      options = [ "subvol=@store" ];
    };

  fileSystems."/var/log" =
    { device = "/dev/disk/by-uuid/ebb73594-4d66-42df-afa2-36417c1069ce";
      fsType = "btrfs";
      options = [ "subvol=@log" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/ebb73594-4d66-42df-afa2-36417c1069ce";
      fsType = "btrfs";
      options = [ "subvol=@home" ];
    };

  fileSystems."/persist" =
    { device = "/dev/disk/by-uuid/ebb73594-4d66-42df-afa2-36417c1069ce";
      fsType = "btrfs";
      options = [ "subvol=@persist" ];
    };

  fileSystems."/var/lib/mysql" =
    { device = "/dev/disk/by-uuid/ebb73594-4d66-42df-afa2-36417c1069ce";
      fsType = "btrfs";
      options = [ "subvol=@mysql" ];
    };

  fileSystems."/var/lib/postgres" =
    { device = "/dev/disk/by-uuid/ebb73594-4d66-42df-afa2-36417c1069ce";
      fsType = "btrfs";
      options = [ "subvol=@postgres" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/33B7-D36F";
      fsType = "vfat";
    };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp1s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
