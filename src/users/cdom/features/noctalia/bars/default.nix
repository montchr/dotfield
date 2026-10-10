{
  users.cdom.aspects.noctalia.home =
    { lib, config, ... }:
    let
      cfg = config.programs.noctalia;

      spacerWidgetSettings = {
        widget = {
          spacer_2 = {
            enabled = false;
            type = "spacer";
          };
          spacer_3 = {
            enabled = false;
            type = "spacer";
          };
          spacer_4 = {
            enabled = false;
            type = "spacer";
          };
          spacer_5 = {
            enabled = false;
            type = "spacer";
          };
        };
      };
    in
    {
      programs.noctalia.settings = spacerWidgetSettings // {
        plugins.enabled = [
          "dotnetrob/cat"
        ];

        widget.cat.type = "dotnetrob/cat:cat";

        widget.clock = {
          capsule = true;
          capsule_padding = 4;
          capsule_radius = 0;
          font_family = "Unifont";
          font_scale = 1;
          format = "{:%F %H:%M:%S}";
          scale = 1.25;
        };

        widget.battery.show_label = false;
        widget.media = {
          album_art_only = true;
          hide_when_no_media = true;
        };
        widget.network.show_label = false;
        widget.volume.show_label = false;

        bar.default = {
          start = [
            "group:start"
            "workspaces"
            "ribbon"
          ];
          center = [ ];
          end = [
            "spacer_4"
            "media"
            "tray"
            "spacer_2"
            "group:controls"
            "spacer_3"
            "clock"
            "cat"
            "spacer_5"
          ];

          border_width = 1;
          capsule_radius = 0;
          concave_edge_corners = false;
          contact_shadow = true;
          margin_ends = 0;
          padding = 6;
          radius = 0;
          thickness = 26;

          capsule_group = [
            {
              accordion = false;
              accordion_direction = "end";
              enabled = true;
              fill = "surface_variant";
              id = "start";
              members = [
                "session"
                "control-center"
                "wallpaper"
                "launcher"
              ];
              opacity = 1;
              padding = 6;
              radius = 0;
            }
            {
              accordion = false;
              accordion_direction = "end";
              enabled = true;
              fill = "surface";
              id = "controls";
              members = [
                "privacy"
                "recorder"
                "clipboard"
                "notifications"
                "caffeine"
                "network"
                "bluetooth"
                "volume"
                "battery"
              ];
              opacity = 1;
              padding = 6;
              radius = 0;
            }
          ];
        };
      };
    };

  users.cdom.aspects.desktop-sessions__niri.home = {
    programs.noctalia.settings = {
      plugins.enabled = [
        "luochen1990/niri-ribbon"
      ];

      widget.ribbon.type = "luochen1990/niri-ribbon:ribbon";
    };
  };
}
