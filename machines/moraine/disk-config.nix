# FIXME: unused, possibly not feasible as of [2023-06-10]: <https://github.com/nix-community/disko/issues/261>
# Source: <https://github.com/srid/nixos-config/blob/afc22aafdeb69cee50e3a2ec2542ed1fb90f72f2/systems/hetzner/disko/two-raids-on-two-disks.nix>
{ lib, ... }:
{
  disk =
    lib.genAttrs
      [
        "/dev/nvme0n1"
        "/dev/nvme1n1"
      ]
      (disk: {
        type = "disk";
        device = disk;
        content = {
          type = "table";
          format = "gpt";
          partitions = [
            {
              name = "boot";
              start = "0";
              end = "2M";
              part-type = "primary";
              flags = [ "bios_grub" ];
            }
            {
              name = "ESP";
              start = "2M";
              end = "1GiB";
              fs-type = "fat32";
              bootable = true;
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            }
            {
              name = "nixos";
              start = "1GiB";
              end = "100%";
              content = {
                type = "btrfs";
                name = "nixos";
              };
            }
          ];
        };
      });
  # mdadm = {
  #   boot = {
  #     type = "mdadm";
  #     level = 1;
  #     metadata = "1.0";
  #     content = {
  #       type = "filesystem";
  #       format = "vfat";
  #       mountpoint = "/boot";
  #     };
  #   };
  #   nixos = {
  #     type = "mdadm";
  #     level = 1;
  #     content = {
  #       type = "filesystem";
  #       format = "ext4";
  #       mountpoint = "/";
  #     };
  #   };
  # };
}
