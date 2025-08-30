{
  hosts.nixos.boschic.configuration = # FIXME: use device labels for interop
    { lib, ... }:
    {
      boot.initrd.availableKernelModules = [
        "nvme"
        "xhci_pci"
        "ahci"
        "usb_storage"
        "usbhid"
        "sd_mod"
      ];
      boot.initrd.kernelModules = [ "kvm-amd" ];
      boot.extraModulePackages = [ ];

      fileSystems."/" = {
        device = "/dev/disk/by-uuid/cba89c2c-fb8a-4335-8ca1-8518808e32eb";
        fsType = "btrfs";
        options = [
          "subvol=root"
          "compress=zstd"
          "noatime"
        ];
      };

      fileSystems."/home" = {
        device = "/dev/disk/by-uuid/cba89c2c-fb8a-4335-8ca1-8518808e32eb";
        fsType = "btrfs";
        options = [
          "subvol=home"
          "compress=zstd"
        ];
      };

      fileSystems."/nix" = {
        device = "/dev/disk/by-uuid/cba89c2c-fb8a-4335-8ca1-8518808e32eb";
        fsType = "btrfs";
        options = [
          "subvol=nix"
          "compress=zstd"
          "noatime"
        ];
      };

      fileSystems."/persist" = {
        device = "/dev/disk/by-uuid/cba89c2c-fb8a-4335-8ca1-8518808e32eb";
        fsType = "btrfs";
        options = [
          "subvol=persist"
          "compress=zstd"
          "noatime"
        ];
      };

      fileSystems."/var/log" = {
        device = "/dev/disk/by-uuid/cba89c2c-fb8a-4335-8ca1-8518808e32eb";
        fsType = "btrfs";
        options = [
          "subvol=log"
          "compress=zstd"
          "noatime"
        ];
        neededForBoot = true;
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/9861-7D46";
        fsType = "vfat";
      };

      swapDevices = [
        # FIXME: apparently systemd will load this automatically? that explains why it won't work when i add it here
        # https://github.com/NixOS/nixpkgs/pull/5202#issuecomment-65257876
        # { device = "/dev/disk/by-uuid/4ffe4cf7-ad2d-4218-9f7c-b4393306636a"; }
      ];

      # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
      # (the default) this is the recommended approach. When using systemd-networkd it's
      # still possible to use this option, but it's recommended to use it in conjunction
      # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
      networking.useDHCP = lib.mkDefault true;
      # networking.interfaces.enp9s0.useDHCP = lib.mkDefault true;
      # networking.interfaces.ts0.useDHCP = lib.mkDefault true;
      # networking.interfaces.wlp7s0.useDHCP = lib.mkDefault true;
    };
}
