# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{ moduleWithSystem, ... }:
{
  users.cdom.aspects.graphical.home = moduleWithSystem (
    perSystem@{ config }:
    { pkgs, ... }:
    let
      mkIosevkaSgrVariant =
        variant:
        pkgs.iosevka-bin.override {
          variant = "SGr-Iosevka${variant}";
        };
    in
    {
      dconf.settings = {
        # FIXME: also set the equivalent settings for fontconfig
        "org/gnome/desktop/interface" = {
          font-antialiasing = "rgba";
          font-hinting = "slight";
          text-scaling-factor = 1.0;
        };
      };
    }
  );
}
