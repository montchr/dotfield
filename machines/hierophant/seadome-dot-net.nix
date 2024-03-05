{ops, ...}: {
  # Matrix Synapse requires well-known data on the base domain. This profile
  # contains the configuration for the base domain, but not the service-specific
  # well-known data.
  services.nginx.virtualHosts.${ops.networks.seadome.domain} = {
    enableACME = true;
    forceSSL = true;
  };
}
