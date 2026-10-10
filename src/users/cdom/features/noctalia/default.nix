{
  users.cdom.aspects.noctalia.home =
    { config, lib, ... }:
    let
      inherit (lib) mkDefault;
      inherit (config.xdg.userDirs) pictures;
    in
    {
      programs.noctalia.settings = {
        location.address = mkDefault "Philadelphia, PA, USA";

        shell.panel.open_near_click_session = true;
        shell.shadow.alpha = 0.45;
        shell.window_switcher.mru = true; # sort by most-recent

        wallpaper = rec {
          directory = "${pictures}/wallpapers/src";
          directory_dark = "${directory}/dark";
          directory_light = "${directory}/light";
        };

        plugins.enabled = [
          "dotnetrob/cat"
          "oldirtty/color_picker"
        ];

        plugin_settings."noctalia/screen_recorder".copy_to_clipboard = true;

        widget.cat.type = "dotnetrob/cat:cat";
      };
    };

  users.cdom.aspects.desktop-sessions__niri.home = {
    programs.noctalia.settings = {
      plugins.enabled = [
        "imjustdoingmypart/niri-animations"
      ];

      backdrop = {
        enabled = true;
        blur_intensity = 0.5;
        tint_intensity = 0.3;
      };
    };
  };
}
