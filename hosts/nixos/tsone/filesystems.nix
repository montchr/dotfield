{
  config,
  lib,
  pkgs,
  ...
}: {
  boot.supportedFilesystems = ["btrfs"];

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [ "subvol=local/root" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [
        "subvol=local/nix"
        "compress=zstd"
        "noatime"
      ];
    };

  fileSystems."/var/log" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [
        "subvol=local/log"
        "noatime"
        "compress=zstd"
      ];
      neededForBoot = true;
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [
        "subvol=safe/home"
        "noatime"
        "compress=zstd"
      ];
    };

  fileSystems."/persist" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [
        "subvol=safe/persist"
        "noatime"
        "compress=zstd"
      ];
      neededForBoot = true;
    };

  fileSystems."/var/lib/postgres" =
    { device = "/dev/disk-by-label/nixos";
      fsType = "btrfs";
      options = [
        "subvol=safe/postgres"
        "compress=zstd"
        "noatime"
        "nofail"
      ];
    };

  fileSystems."/silo/backup" =
    { device = "/dev/disk/by-label/silo";
      fsType = "btrfs";
      options = [
        "subvol=backup"
        "compress=zstd"
        "noatime"
        "nofail"
      ];
    };

  fileSystems."/silo/data" =
    { device = "/dev/disk/by-label/silo";
      fsType = "btrfs";
      options = [
        "subvol=data/media"
        "compress=zstd"
        "nofail"
      ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/CA0D-4891";
      fsType = "vfat";
      # options = ["nofail" "X-mount.mkdir"];
    };

  boot.initrd.luks.devices = {
    "enc01" = {
      device = "/dev/disk/by-uuid/99b7edb7-e79f-423a-b043-fe3534cf8132";
    };
    "enc02" = {
      device = "/dev/disk/by-uuid/34fd5e9a-be69-4537-97fd-650249e22117";
    };
    "enc03" = {
      device = "/dev/disk/by-uuid/0978aaa3-fabf-4140-b9af-7b6fa2bd2e8a";
    };
    "enc04" = {
      device = "/dev/disk/by-uuid/05a35847-7bf6-446c-8062-a34a4c3814e8";
    };
    "enc05" = {
      device = "/dev/disk/by-uuid/2dcdd2d2-2e3b-44f6-ae08-6163ea93d1a7";
    };
    "enc06" = {
      device = "/dev/disk/by-uuid/b2c88455-d749-4088-aa70-e0010268eccc";
    };
    "enc07" = {
      device = "/dev/disk/by-uuid/e89c848c-c880-414f-a9f1-404fdedecad3";
    };
    "enc08" = {
      device = "/dev/disk/by-uuid/791a538f-0f8d-4cdf-a984-2f785e9bcc72";
    };
    "enc09" = {
      device = "/dev/disk/by-uuid/76b9cd36-8eff-4047-9b05-cc5c0e496bbf";
    };
    "enc10" = {
      device = "/dev/disk/by-uuid/c8a4f871-41b6-46c7-9e6b-ec7cd0cd2869";
    };
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
      device = "/dev/disk/by-partuuid/f163bf8c-3760-476d-b490-7cd42d4452ac";
      randomEncryption.enable = true;
    }
    {
      device = "/dev/disk/by-partuuid/6ecad196-ac69-48de-8353-21afd33d2e89";
      randomEncryption.enable = true;
    }
  ];


}
