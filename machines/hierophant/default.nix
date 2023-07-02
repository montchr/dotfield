{lib, ...}: {
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./headscale.nix
    ./networking.nix
    ./secrets/sops.nix
    ./users.nix
  ];

  system.stateVersion = "23.05";
}
