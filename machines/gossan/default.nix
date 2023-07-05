{flake, ...}: let
  inherit (flake.inputs) disko;
in {
  imports = [
    disko.nixosModules.disko
    (import ./disk-config.nix {
      disks = ["/dev/sda"];
    })

    ./secrets/sops.nix
    ./users.nix
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "virtio_pci" "virtio_scsi" "usbhid" "sr_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = [];
  boot.extraModulePackages = [];

  system.stateVersion = "23.05";
}
