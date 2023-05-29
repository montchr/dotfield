let
  commonOpts = ["noatime" "x-mount.mkdir" "compress=zstd"];
in {
  boot.supportedFilesystems = ["btrfs"];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@root" "ssd"];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@store" "ssd"];
  };

  fileSystems."/var/log" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options =
      commonOpts ++ ["subvol=@log" "ssd"];
    neededForBoot = true;
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@home" "ssd"];
  };

  fileSystems."/persist" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@persist" "ssd"];
    neededForBoot = true;
  };

  fileSystems."/var/lib/mysql" = {
    device = "/dev/disk-by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@mysql" "ssd" "nofail"];
  };

  fileSystems."/var/lib/postgres" = {
    device = "/dev/disk-by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@postgres" "ssd" "nofail"];
  };

  fileSystems."/local/backups" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@backups" "nofail"];
  };

  fileSystems."/local/downloads/completed" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@downloads-completed" "nofail"];
  };

  fileSystems."/local/media/music" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@music" "nofail"];
  };

  fileSystems."/local/media/movies" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@movies" "nofail"];
  };

  fileSystems."/local/media/tv-shows" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@tv-shows" "nofail"];
  };

  fileSystems."/boot" = {
    # FIXME: uuid
    # device = "/dev/disk/by-uuid/CA0D-4891";
    fsType = "vfat";
    # options = ["nofail" "X-mount.mkdir"];
  };
}
