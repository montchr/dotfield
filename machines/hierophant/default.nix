{...}: {
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./networking.nix
    ./secrets/sops.nix
    ./users.nix

    ./acme.nix
    ./grafana.nix
    ./prometheus.nix
  ];

  system.stateVersion = "23.05";
}
