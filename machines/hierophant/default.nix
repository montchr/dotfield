{lib, ...}: {
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./networking.nix
    ./secrets/sops.nix
    ./users.nix
  ];

  system.stateVersion = "23.05";
}
