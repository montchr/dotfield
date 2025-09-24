{ moduleWithSystem, ... }:
{
  aspects.desktop-sessions__wayland-wm.home = moduleWithSystem (
    perSystem@{ config }:
    {
      services.dunst.enable = true;
      home.packages = [ perSystem.config.packages.dunst-meter-notify ];
    }
  );
}
