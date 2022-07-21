{
  config,
  lib,
  pkgs,
  ...
}:
let
  commonOpts = [
    "noatime"
    "x-mount.mkdir"
    "compress=zstd"
  ];
in
{
  boot.supportedFilesystems = ["btrfs"];

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

  fileSystems."/var/lib/postgres" = {
    device = "/dev/disk-by-label/nixos";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@postgres"
      "ssd"
      "nofail"
    ];
  };

  fileSystems."/silo/backups" = {
    device = "/dev/disk/by-label/silo";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@backups"
      "nofail"
    ];
  };

  fileSystems."/silo/downloads/completed" = {
    device = "/dev/disk/by-label/silo";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@dl-completed"
      "nofail"
    ];
  };

  fileSystems."/silo/media/music" = {
    device = "/dev/disk/by-label/silo";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@music"
      "nofail"
    ];
  };

  fileSystems."/silo/media/movies" = {
    device = "/dev/disk/by-label/silo";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@movies"
      "nofail"
    ];
  };

  fileSystems."/silo/media/tv-shows" = {
    device = "/dev/disk/by-label/silo";
    fsType = "btrfs";
    options = commonOpts ++ [
      "subvol=@tv-shows"
      "nofail"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/CA0D-4891";
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

  # Note that `/dev/disk/by-uuid` is not compatible with `randomEncryption`
  # because the UUID will change on every boot.
  # https://github.com/NixOS/nixpkgs/blob/c06d5fa9c605d143b15cafdbbb61c7c95388d76e/nixos/modules/config/swap.nix#L24-L26
  swapDevices = [
    {
      device = "/dev/disk/by-uuid/5881331f-7c23-452b-8562-c9103098dce8";
      # randomEncryption.enable = true;
    }
    {
      device = "/dev/disk/by-uuid/80b1fe3d-96f0-45c6-9787-80de4570906c";
      # randomEncryption.enable = true;
    }
  ];
}
