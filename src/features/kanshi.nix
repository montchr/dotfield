# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# <https://sr.ht/~emersion/kanshi/>
#
# > kanshi allows you to define output profiles that are automatically
# > enabled and disabled on hotplug. For instance, this can be used to
# > turn a laptop's internal screen off when docked.
# >
# > This is a Wayland equivalent for tools like autorandr. kanshi can be
# > used on Wayland compositors supporting the wlr-output-management
# > protocol.
{
  aspects.desktop-sessions__wayland-wm.home =
    { config, ... }:
    let
      cfg = config.services.kanshi;
    in
    {
      services.kanshi = {
        settings = [
          {
            output.criteria = "LG Electronics LG Ultra HD 0x000668B9";
            output.scale = 2.0;
            output.mode = "3840x2160";
          }
          {
            output.criteria = "LG Electronics LG UltraFine 901NTEPB3220";
            output.scale = 2.0;
            output.mode = "3840x2160";
          }
          {
            output.criteria = "LG Electronics LG ULTRAGEAR 107NTBKA5869";
            output.scale = 1.0;
            output.mode = "2560x1440";
          }
          {
            output.criteria = "Acer Technologies CB241Y 0x33340DC8F";
            output.scale = 1.0;
            output.mode = "1920x1080";
            output.transform = "270";
          }
        ];
      };

      home.packages = [ cfg.package ];
    };
}
