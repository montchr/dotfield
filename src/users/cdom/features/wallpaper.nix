{ moduleWithSystem, ... }:
{
  users.cdom.aspects.desktop-sessions__wayland-wm.home = moduleWithSystem (
    perSystem@{ config }:
    {
      services.swww.enable = true;
      home.packages = [
        perSystem.config.packages.swww-randomize
      ];
    }
  );
}
