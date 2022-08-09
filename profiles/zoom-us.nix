{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [zoom-us];
  home-manager.sharedModules = [
    (lib.mkIf config.services.xserver.displayManager.gdm.wayland {
      xdg.configFile."zoomus.conf".text = ''
        enableWaylandShare=true
      '';
    })
  ];
}
