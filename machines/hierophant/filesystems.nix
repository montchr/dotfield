let
  commonOpts = [
    "noatime"
    "x-mount.mkdir"
    "compress=zstd"
  ];
in
{
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [ "subvol=@root" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [ "subvol=@store" ];
  };

  fileSystems."/var/log" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [ "subvol=@log" ];
    neededForBoot = true;
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [ "subvol=@home" ];
  };

  fileSystems."/persist" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [ "subvol=@persist" ];
    neededForBoot = true;
  };

  fileSystems."/var/lib/mysql" = {
    device = "/dev/disk-by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@mysql"
      "nofail"
    ];
  };

  fileSystems."/var/lib/postgres" = {
    device = "/dev/disk-by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@postgres"
      "nofail"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/33B7-D36F";
    fsType = "vfat";
  };
}
