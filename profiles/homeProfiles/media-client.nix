moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfield.features) hasWayland;
  hasNvidia = moduleArgs.osConfig.dotfield.features.hasNvidia or false;
in {
  imports = [
    ./mpv.nix
    ./jellyfin-client.nix
    ./plex-client.nix
  ];
}
