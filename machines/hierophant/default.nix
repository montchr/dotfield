{...}: {
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./postgresql.nix
    ./networking.nix
    ./secrets/sops.nix
    ./users.nix

    ./acme.nix
    ./seadome-dot-net.nix
    # ./atticd.nix

    ./monitoring/grafana/default.nix
    ./monitoring/grafana/oauth.nix
    ./monitoring/loki.nix
    ./monitoring/prometheus.nix
    ./monitoring/promtail.nix

    #: applications
    ./keycloak.nix
    ./matrix/synapse.nix
  ];

  system.stateVersion = "23.05";
}
