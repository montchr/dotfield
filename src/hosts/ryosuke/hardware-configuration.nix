{
  dotfield.hosts.nixos.ryosuke.nixos =
    {
      config,
      lib,
      modulesPath,
      ...
    }:
    {
      imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

      # FIXME: this is untest!  the definition was missing.  ryosuke
      # needs a checkup.
      boot.loader.grub.devices = [ config.fileSystems."/boot".device ];

      boot.initrd.availableKernelModules = [
        "nvme"
        "xhci_pci"
        "ahci"
        "usb_storage"
        "usbhid"
        "sd_mod"
      ];
      boot.initrd.kernelModules = [ "dm-snapshot" ];
      boot.kernelModules = [ "kvm-amd" ];
      boot.extraModulePackages = [ ];

      boot.initrd.luks.devices."enc" = {
        device = "/dev/disk/by-uuid/6a7e6a45-704f-4938-bf3a-a0fce928d91f";
        preLVM = true;
      };

      fileSystems."/" = {
        device = "/dev/disk/by-label/nixos";
        fsType = "btrfs";
        options = [
          "compress=zstd"
          "noatime"
          "subvol=@root"
        ];
      };

      fileSystems."/nix" = {
        device = "/dev/disk/by-label/nixos";
        fsType = "btrfs";
        options = [
          "compress=zstd"
          "noatime"
          "subvol=@store"
        ];
      };

      fileSystems."/var/log" = {
        device = "/dev/disk/by-label/nixos";
        fsType = "btrfs";
        options = [
          "compress=zstd"
          "noatime"
          "subvol=@log"
        ];
      };

      fileSystems."/home" = {
        device = "/dev/disk/by-label/nixos";
        fsType = "btrfs";
        options = [
          "compress=zstd"
          "noatime"
          "subvol=@home"
        ];
      };

      fileSystems."/persist" = {
        device = "/dev/disk/by-label/nixos";
        fsType = "btrfs";
        options = [
          "compress=zstd"
          "noatime"
          "subvol=@persist"
        ];
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/B368-8A84";
        fsType = "vfat";
      };

      swapDevices = [ { device = "/dev/disk/by-uuid/0d88632f-cfef-4416-9b45-78bcc9ad3a77"; } ];

      # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
      # (the default) this is the recommended approach. When using systemd-networkd it's
      # still possible to use this option, but it's recommended to use it in conjunction
      # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
      networking.useDHCP = lib.mkDefault true;
      networking.interfaces.enp4s0.useDHCP = lib.mkDefault true;

      hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

      nixpkgs.hostPlatform = "x86_64-linux";
    };
}
