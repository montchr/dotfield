{ lib, ... }:
{
  dconf.settings."org/gnome/desktop/peripherals/touchpad" = {
    # Weirdly enough, some people prefer natural scroll.
    natural-scroll = false;
    tap-to-click = true;
    two-finger-scrolling-enabled = true;
  };
  wayland.windowManager.sway.config.input."type:touchpad" = {
    tap = "enabled";
    natural_scroll = "disabled";
  };
}
