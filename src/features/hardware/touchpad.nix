{ lib, ... }:
{
  aspects.touchpad.home = {
    dconf.settings."org/gnome/desktop/peripherals/touchpad" = {
      two-finger-scrolling-enabled = lib.mkDefault true;
    };
  };
}
