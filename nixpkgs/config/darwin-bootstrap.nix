{ config, pkgs, lib, ... }:

{
  imports = [
    ./shared.nix
  ];

  nix = {
    trustedUsers = [
      "@admin"
    ];
  };

  services = {
    nix-daemon.enable = true;
  };

  users.nix.configureBuildUsers = true;

  system.stateVersion = 4;
}
