{
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./headscale.nix
    ./users.nix
  ];

  system.stateVersion = "23.05";
}
