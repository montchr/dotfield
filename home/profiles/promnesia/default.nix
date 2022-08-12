# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText: Copyright (c) 2022 GuangTao Zhang
# SPDX-License-Identifier: MIT
#
## Sources:
#
# https://github.com/GTrunSec/nixos-flk/blob/96ce0881a2185261758c0ad206d4149ad47d5b04/profiles/data/promnesia/default.nix
# https://github.com/karlicoss/promnesia/issues/137
{
  config,
  lib,
  pkgs,
  ...
}: let
  # readConfig = builtins.readFile ./config.py;
  # configFile = pkgs.writeScript "config.py" readConfig;
  # watcherPath = "/home/gtrun/Dropbox/org-notes/braindump";
in {
  home.packages = with pkgs; [promnesia];
  # systemd.user.services.promnesia = {
  #   description = "promnesia Daemon";
  #   wantedBy = ["graphical-session.target"];
  #   preStart = ''
  #     ${pkgs.promnesia}/bin/promnesia index --config ${configFile}
  #   '';
  #   serviceConfig = {
  #     ExecStart = ''
  #       ${pkgs.promnesia}/bin/promnesia serve
  #     '';
  #     Restart = "on-failure";
  #   };
  # };

  # systemd.user.paths.promnesia-watcher = {
  #   wantedBy = ["promnesia.service"];
  #   pathConfig = {
  #     PathChanged = [watcherPath];
  #     Unit = "promnesia-restarter.service";
  #   };
  # };

  # systemd.user.services.promnesia-restarter = {
  #   serviceConfig.Type = "oneshot";
  #   wantedBy = ["promnesia.service"];
  #   script = ''
  #     systemctl --user restart promnesia.service
  #   '';
  # };
}
