{ lib, ... }:
{
  dotfield.features."hardware/touchpad".home = {
    dconf.settings."org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click = lib.mkDefault true;
      two-finger-scrolling-enabled = lib.mkDefault true;
    };
    wayland.windowManager.sway.config.input."type:touchpad" = {
      tap = lib.mkDefault "enabled";
    };
  };
}
