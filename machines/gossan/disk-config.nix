{disks ? ["/dev/sda"], ...}: {
  disko.devices.disk.main = {
    type = "disk";
    device = builtins.elemAt disks 0;
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          size = "512M";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
          };
        };
        root = {
          size = "100%";
          content = {
            type = "btrfs";
            extraArgs = ["-f"];
            subvolumes = builtins.mapAttrs (_n: v: v // {mountOptions = ["compress=zstd" "noatime"];}) {
              "/@root".mountpoint = "/";
              "/@home".mountpoint = "/home";
              "/@store".mountpoint = "/nix";
              "/@log".mountpoint = "/var/log";
              "/@persist".mountpoint = "/persist";
              "/@mysql".mountpoint = "/var/lib/mysql";
              "/@postgres".mountpoint = "/var/lib/postgres";
            };
          };
        };
      };
    };
  };
}
