{
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./headscale.nix
    ./users.nix
  ];

  security.acme = {
    # TODO: move to central location
    defaults.email = "ops@seadome.net";
    acceptTerms = true;
  };

  system.stateVersion = "23.05";
}
