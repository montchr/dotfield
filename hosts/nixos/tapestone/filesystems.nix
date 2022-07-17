{
  config,
  lib,
  pkgs,
  ...
}: {
  # Required for ZFS.
  networking.hostId = "e2e496d7";

  # FIXME: configure mail sending
  nixpkgs.config.packageOverrides = pkgs: {
    zfsStable = pkgs.zfsStable.override {enableMail = true;};
    zfsUnstable = pkgs.zfsUnstable.override {enableMail = true;};
  };

  boot.zfs.enableUnstable = true;
  # TODO: the manual recommends setting this to false for improved zfs
  # safeguards, but it's enabled by default for compatibility
  # zfs.forceImportRoot = false;

  boot.supportedFilesystems = ["btrfs" "zfs"];

  swapDevices = [];

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [ "subvol=local/root" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [ "subvol=local/nix" "noatime" ];
    };

  fileSystems."/var/log" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [ "subvol=local/log" "nofail"];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [ "subvol=safe/home" ];
    };

  fileSystems."/persist" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [ "subvol=safe/persist" ];
    };

  fileSystems."/var/lib/postgres" =
    { device = "/dev/disk-by-label/nixos";
      fsType = "btrfs";
      options = [ "subvol=safe/postgres" "nofail" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/CA0D-4891";
      fsType = "vfat";
      # options = ["nofail" "X-mount.mkdir"];
    };

  # fileSystems."/boot-fallback" = {
  #   device = "/dev/disk/by-uuid/FF5D-559E";
  #   fsType = "vfat";
  #   # Continue booting regardless of the availability of the mirrored boot
  #   # partitions. We don't need both.
  #   options = ["nofail" "X-mount.mkdir"];
  # };

  fileSystems."/silo/backup" = {
    device = "spool/backup";
    fsType = "zfs";
    options = ["nofail" "zfsutil"];
  };

  fileSystems."/silo/data" = {
    device = "spool/data";
    fsType = "zfs";
    options = ["nofail" "zfsutil"];
  };
}
