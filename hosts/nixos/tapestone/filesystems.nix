{
  config,
  lib,
  pkgs,
  ...
}: {
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/D94C-E69E";
    fsType = "vfat";

    options = ["nofail"];
  };

  fileSystems."/boot-fallback" = {
    device = "/dev/disk/by-uuid/D94D-4F3B";
    fsType = "vfat";
    # Continue booting regardless of the availability of the mirrored boot
    # partitions. We don't need both.
    options = ["nofail"];
  };

  fileSystems."/" = {
    device = "rpool/local/root";
    fsType = "zfs";
  };

  fileSystems."/nix" = {
    device = "rpool/local/nix";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "rpool/safe/home";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/persist" = {
    device = "rpool/safe/persist";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/var/lib/postgres" = {
    device = "rpool/safe/postgres";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/silo/backup" = {
    device = "spool/backup";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/silo/data" = {
    device = "spool/data";
    fsType = "zfs";
    options = ["nofail"];
  };
}
