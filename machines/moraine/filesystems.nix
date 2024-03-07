let
  commonOpts = [
    "noatime"
    "x-mount.mkdir"
    "compress=zstd"
  ];
in
{
  boot.supportedFilesystems = [ "btrfs" ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@root"
      "ssd"
    ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@store"
      "ssd"
    ];
  };

  fileSystems."/var/log" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@log"
      "ssd"
    ];
    neededForBoot = true;
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@home"
      "ssd"
    ];
  };

  fileSystems."/persist" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@persist"
      "ssd"
    ];
    neededForBoot = true;
  };

  fileSystems."/var/lib/mysql" = {
    device = "/dev/disk-by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@mysql"
      "ssd"
      "nofail"
    ];
  };

  fileSystems."/var/lib/postgres" = {
    device = "/dev/disk-by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@postgres"
      "ssd"
      "nofail"
    ];
  };

  fileSystems."/mnt/local/backups" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@backups"
      "nofail"
    ];
  };

  fileSystems."/mnt/local/downloads/completed" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@completed"
      "nofail"
    ];
  };

  fileSystems."/mnt/local/downloads/torrents" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@torrents"
      "nofail"
    ];
  };

  fileSystems."/mnt/local/Media" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@media"
      "nofail"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  # Note that `/dev/disk/by-uuid` is not compatible with `randomEncryption`
  # because the UUID will change on every boot.
  # https://github.com/NixOS/nixpkgs/blob/c06d5fa9c605d143b15cafdbbb61c7c95388d76e/nixos/modules/config/swap.nix#L24-L26
  # FIXME: update uuids
  # swapDevices = [
  #   {
  #     # device = "/dev/disk/by-uuid/5881331f-7c23-452b-8562-c9103098dce8";
  #     # randomEncryption.enable = true;
  #   }
  #   {
  #     # device = "/dev/disk/by-uuid/80b1fe3d-96f0-45c6-9787-80de4570906c";
  #     # randomEncryption.enable = true;
  #   }
  # ];
}
