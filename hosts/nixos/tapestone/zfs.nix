{
  config,
  lib,
  pkgs,
  ...
}: {
  boot.supportedFilesystems = ["zfs"];
  boot.zfs.enableUnstable = true;
  boot.zfs.requestEncryptionCredentials = true;

  # FIXME: configure mail sending
  nixpkgs.config.packageOverrides = pkgs: {
    zfsStable = pkgs.zfsStable.override {enableMail = true;};
    zfsUnstable = pkgs.zfsUnstable.override {enableMail = true;};
  };

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
}
