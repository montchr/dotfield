{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfield.features) hasWayland;
in {
  environment.systemPackages = with pkgs; [zoom-us];
  home-manager.sharedModules = [
    {
      # FIXME: does this prevent the gui from managing preferences?
      xdg.configFile."zoomus.conf".text = ''
        ${lib.optionalString hasWayland "enableWaylandShare=true"}
      '';
    }
  ];
}
