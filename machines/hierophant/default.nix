{lib, ...}: {
  imports = [
    ./headscale.nix
    ./users.nix
    ./hardware-configuration.nix
  ];

  security.acme = {
    # TODO: move to central location
    defaults.email = "ops@seadome.net";
    acceptTerms = true;
  };

  services.tailscale = {
    enable = true;
    useRoutingFeatures = lib.mkDefault "client";
  };

  system.stateVersion = "23.05";
}
