{ lib, ... }:
{
  dconf.settings."org/gnome/desktop/peripherals/touchpad" = {
    # Weirdly enough, some people prefer natural scroll.
    natural-scroll = lib.mkDefault false;
    tap-to-click = lib.mkDefault true;
    two-finger-scrolling-enabled = true;
  };
}
