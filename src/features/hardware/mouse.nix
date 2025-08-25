{ lib, ... }:
{
  dotfield.aspects.graphical.home = {
    dconf.settings."org/gnome/desktop/peripherals/mouse" = {
      accel-profile = lib.mkDefault "adaptive";
    };
  };
}
