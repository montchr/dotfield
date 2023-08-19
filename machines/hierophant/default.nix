{...}: {
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./networking.nix
    ./secrets/sops.nix
    ./users.nix

    ./acme.nix
    ./seadome-dot-net.nix

    # ./atticd.nix
    ./monitoring/grafana.nix
    ./monitoring/loki.nix
    ./monitoring/prometheus.nix
    ./monitoring/promtail.nix

    #: applications
    ./matrix/synapse.nix
  ];

  system.stateVersion = "23.05";
}
