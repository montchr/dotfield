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
    extraPackages = with pkgs; [podman-compose];
  };
  users.users.${guardian.username}.extraGroups = ["podman"];
}
