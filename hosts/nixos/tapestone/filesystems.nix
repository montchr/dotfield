{
  config,
  lib,
  pkgs,
  ...
}: {
  # Required for ZFS.
  networking.hostId = "93e48b92";

  # FIXME: configure mail sending
  nixpkgs.config.packageOverrides = pkgs: {
    zfsStable = pkgs.zfsStable.override {enableMail = true;};
    zfsUnstable = pkgs.zfsUnstable.override {enableMail = true;};
  };

  boot.zfs.enableUnstable = true;
  boot.zfs.devNodes = "/dev/disk/by-id";
  boot.supportedFilesystems = ["zfs"];

  # ZFS maintenance settings.
  services.zfs.trim.enable = true;
  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoScrub.enable = true;
  services.zfs.autoScrub.pools = ["rpool" "spool"];

  # ZFS already has its own scheduler.
  # https://nixos.wiki/wiki/ZFS#How_to_use_it
  services.udev.extraRules = ''
    ACTION=="add|change", KERNEL=="sd[a-z]*[0-9]*|mmcblk[0-9]*p[0-9]*|nvme[0-9]*n[0-9]*p[0-9]*", ENV{ID_FS_TYPE}=="zfs_member", ATTR{../queue/scheduler}="none"
  '';

  swapDevices = [];

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/D94C-E69E";
    fsType = "vfat";
    options = ["nofail" "X-mount.mkdir"];
  };

  fileSystems."/boot-fallback" = {
    device = "/dev/disk/by-uuid/D94D-4F3B";
    fsType = "vfat";
    # Continue booting regardless of the availability of the mirrored boot
    # partitions. We don't need both.
    options = ["nofail" "X-mount.mkdir"];
  };

  fileSystems."/" = {
    device = "rpool/local/root";
    fsType = "zfs";
    options = ["zfsutil"];
  };

  fileSystems."/nix" = {
    device = "rpool/local/nix";
    fsType = "zfs";
    options = ["zfsutil"];
  };

  fileSystems."/home" = {
    device = "rpool/safe/home";
    fsType = "zfs";
    options = ["nofail" "zfsutil"];
  };

  fileSystems."/persist" = {
    device = "rpool/safe/persist";
    fsType = "zfs";
    options = ["nofail" "zfsutil"];
  };

  fileSystems."/var/lib/postgres" = {
    device = "rpool/safe/postgres";
    fsType = "zfs";
    options = ["nofail" "zfsutil"];
  };

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
