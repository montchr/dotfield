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
    options = commonOpts ++ ["subvol=@postgres" "ssd"];
  };

  fileSystems."/srv/backups" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@backups" "nofail"];
  };

  fileSystems."/srv/data/torrents/completed" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@bt-completed"];
  };

  fileSystems."/srv/data/torrents/incoming" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@bt-incoming"];
  };

  fileSystems."/srv/data/torrents/metadata" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@bt-metadata"];
  };

  fileSystems."/srv/data/torrents/watch" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@bt-watch"];
  };

  fileSystems."/srv/media/incoming" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@media-incoming"];
  };

  fileSystems."/srv/media/outgoing" = {
    device = "/dev/disk/by-label/local";
    fsType = "btrfs";
    options = commonOpts ++ ["subvol=@media-outgoing"];
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
