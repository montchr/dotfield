# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

# FIXME: too many assumptions

{
  users.cdom.aspects.desktop-sessions__wayland-wm.home =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      swaylock = lib.getExe config.programs.swaylock.package;
      pgrep = "${pkgs.procps}/bin/pgrep";
      lockTime = 10 * 60;
    in
    {
      services.swayidle = {
        # enable = true;

        # FIXME: this isn't quite right, but the daemon needs to be
        # activated after the session is ready.  the defaults aren't
        # cutting it.
        systemdTargets = [ "tray.target" ];
        timeouts = [
          {
            timeout = lockTime;
            command = "${swaylock} --daemonize";
          }
        ];
      };
    };
}
