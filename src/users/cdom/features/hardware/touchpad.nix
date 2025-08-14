{
  dotfield.features."hardware/touchpad".home = {
    dconf.settings."org/gnome/desktop/peripherals/touchpad" = {
      natural-scroll = false;
    };
    wayland.windowManager.sway.config.input."type:touchpad" = {
      natural_scroll = "disabled";
    };
  };
}
