{lib, ...}: {
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./headscale.nix
    ./sops.nix
    ./users.nix
  ];

  networking.useDHCP = lib.mkDefault true;

  system.stateVersion = "23.05";
}
