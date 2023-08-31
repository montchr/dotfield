{...}: {
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./networking.nix
    ./secrets/sops.nix
    ./users.nix

    ./acme.nix
    ./postgresql.nix
    ./seadome-dot-net.nix
    ./borgbackup.nix

    # ./atticd.nix
    ./keycloak.nix
    ./matrix/synapse.nix

    ./monitoring/grafana/default.nix
    ./monitoring/grafana/oauth.nix
    ./monitoring/loki.nix
    ./monitoring/prometheus.nix
    ./monitoring/promtail.nix
  ];

  system.stateVersion = "23.05";
}
