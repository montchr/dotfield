{...}: {
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./networking.nix
    ./secrets/sops.nix
    ./users.nix

    ./acme.nix
    # ./atticd.nix
    ./seadome-dot-net.nix

    #: applications
    ./grafana.nix
    ./matrix/synapse.nix
    ./prometheus.nix

    ./monitoring/loki.nix
    ./monitoring/promtail.nix
  ];

  system.stateVersion = "23.05";
}
