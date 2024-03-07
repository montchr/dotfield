{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.dotfield.features) hasWayland;
  inherit (pkgs.stdenv.hostPlatform) isAarch64;
in
{
  environment.systemPackages = lib.optional (!isAarch64) pkgs.zoom-us; # <- broken
  home-manager.sharedModules = [
    {
      # TODO: does this prevent the gui from managing preferences?
      xdg.configFile."zoomus.conf".text = ''
        ${lib.optionalString hasWayland "enableWaylandShare=true"}
      '';
    }
  ];
}
