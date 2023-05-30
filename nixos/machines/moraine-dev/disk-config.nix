# Source: <https://github.com/srid/nixos-config/blob/afc22aafdeb69cee50e3a2ec2542ed1fb90f72f2/systems/hetzner/disko/two-raids-on-two-disks.nix>
{lib, ...}: {
  disk =
    lib.genAttrs [
      "/dev/sda"
      "/dev/disk/by-uuid/fa8d09db-8718-414f-91f8-4529172d9a38" # <- /dev/sdb :: scsi-SHC_Volume_32566072
    ]
    (device: {
      inherit device;
      type = "disk";
      content = {
        type = "table";
        format = "gpt";
        partitions = [
          {
            name = "boot";
            start = "0";
            end = "2M";
            part-type = "primary";
            flags = ["bios_grub"];
          }
          {
            name = "ESP";
            start = "2M";
            end = "2GiB";
            fs-type = "fat32";
            bootable = true;
            content = {
              type = "mdraid";
              name = "boot";
            };
          }
          {
            name = "nixos";
            start = "2GiB";
            end = "100%";
            content = {
              type = "btrfs";
              name = "nixos";
            };
          }
        ];
      };
    });
  mdadm = {
    boot = {
      type = "mdadm";
      level = 1;
      metadata = "1.0";
      content = {
        type = "filesystem";
        format = "vfat";
        mountpoint = "/boot";
      };
    };
    nixos = {
      type = "mdadm";
      level = 1;
      content = {
        type = "filesystem";
        format = "ext4";
        mountpoint = "/";
      };
    };
  };
}
