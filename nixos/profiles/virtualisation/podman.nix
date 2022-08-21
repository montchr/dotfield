{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfield) guardian;
in {
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
  users.users.${guardian.username}.extraGroups = ["podman"];
}
