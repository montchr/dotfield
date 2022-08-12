# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText: Copyright (c) 2022 GuangTao Zhang
# SPDX-License-Identifier: MIT
#
## Sources:
#
# https://github.com/GTrunSec/nixos-flk/blob/96ce0881a2185261758c0ad206d4149ad47d5b04/modules/nixos/promnesia/promnesia.nix
# https://github.com/karlicoss/promnesia/issues/137
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.promnesia;
  # FIXME: set up config file
  # https://github.com/GTrunSec/nixos-flk/blob/main/profiles/data/promnesia/config.py
  configFile = pkgs.writeScript "config.py" cfg.config;
  PreShell = pkgs.writeScript "preRun-promnesia" ''
    if [ ! -d "$HOME/.local/share/promnesia.sqlite" ];then
       ${cfg.package}/bin/promnesia index --config ${configFile}
       fi
  '';
in {
  options.services.promnesia = {
    enable = mkOption {
      description = "Whether to enable promnesia.";
      default = false;
      type = types.bool;
    };

    watcherPath = mkOption {
      type = types.path;
      default = "";
      description = ''
        if this path modified that will restart promnesia service automaticlly.
      '';
    };

    dbPath = mkOption {
      type = types.path;
      default = "";
      description = ''
        sqlite directory for promnesia
      '';
    };
    config = mkOption {
      description = "write resource to config.py";
      default = ''
      '';
      type = types.str;
    };

    package = mkOption {
      description = "promnesia package to use.";
      default = pkgs.promnesia;
      type = types.package;
    };
  };
  # FIXME
  # config = mkIf cfg.enable {
  #   environment.systemPackages = [cfg.package];
  #   systemd.user.services.promnesia =
  #     {
  #       description = "promnesia Daemon";
  #       preStart = ''
  #         ${pkgs.bash}/bin/bash ${PreShell}
  #       '';
  #       serviceConfig = {
  #         ExecStart = mkIf cfg.enable ''
  #           ${cfg.package}/bin/promnesia serve
  #         '';
  #         Restart = "always";
  #       };
  #     }
  #     // optionalAttrs cfg.enable {wantedBy = ["default.target"];};

  #   systemd.user.paths.promnesia-watcher = {
  #     wantedBy = ["promnesia.service"];
  #     pathConfig = {
  #       PathModified = cfg.watcherPath;
  #       Unit = "promnesia-restarter.service";
  #     };
  #   };
  #   systemd.user.services.promnesia-restarter = {
  #     serviceConfig.Type = "oneshot";
  #     wantedBy = ["promnesia.service"];
  #     script = ''
  #       systemctl --user restart promnesia.service
  #     '';
  #   };
  # };
}
