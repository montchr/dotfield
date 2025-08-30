{
  hosts.nixos.hodgepodge.configuration =
    {
      config,
      lib,
      modulesPath,
      ...
    }:
    {
      imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

      boot.initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "usbhid"
        "usb_storage"
        "sd_mod"
      ];
      boot.initrd.kernelModules = [ "dm-snapshot" ];
      boot.kernelModules = [
        "kvm-intel"
        "wl"
      ];
      boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

      boot.initrd.luks.devices."enc" = {
        device = "/dev/disk/by-uuid/55a9158b-6ccd-49be-aab8-2e5e79a7f33b";
        preLVM = true;
      };

      fileSystems."/" = {
        device = "/dev/disk/by-uuid/f793b58b-a84d-481d-a924-ab0a97475da6";
        fsType = "btrfs";
        options = [
          "subvol=root"
          "compress=zstd"
          "noatime"
        ];
      };

      fileSystems."/home" = {
        device = "/dev/disk/by-uuid/f793b58b-a84d-481d-a924-ab0a97475da6";
        fsType = "btrfs";
        options = [
          "subvol=home"
          "compress=zstd"
          "noatime"
        ];
      };

      fileSystems."/nix" = {
        device = "/dev/disk/by-uuid/f793b58b-a84d-481d-a924-ab0a97475da6";
        fsType = "btrfs";
        options = [
          "subvol=nix"
          "compress=zstd"
          "noatime"
        ];
      };

      fileSystems."/persist" = {
        device = "/dev/disk/by-uuid/f793b58b-a84d-481d-a924-ab0a97475da6";
        fsType = "btrfs";
        options = [
          "subvol=persist"
          "compress=zstd"
          "noatime"
        ];
      };

      fileSystems."/var/log" = {
        device = "/dev/disk/by-uuid/f793b58b-a84d-481d-a924-ab0a97475da6";
        fsType = "btrfs";
        options = [
          "subvol=log"
          "compress=zstd"
          "noatime"
        ];
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/C066-6135";
        fsType = "vfat";
      };

      swapDevices = [ { device = "/dev/disk/by-uuid/c3f90838-2c16-4b7c-8123-4e87677b1521"; } ];

      hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    };
}
