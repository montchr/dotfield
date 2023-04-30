# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  config,
  lib,
  ...
} @ moduleArgs: let
  inherit (lib) mkOption;
  inherit (lib.types) str;
  inherit (config.home) username;
  cfg = config.dotfield.paths;
  # FIXME: this will break! needs an `or`... but therein lies a rabbit hole...
  fsPath = moduleArgs.osConfig.dotfield.paths.fsPath;
  userDirsSubmodule = {options, ...}: {
    options = {
      basePath = mkOption {
        type = str;
      };
      configsPath = mkOption {
        type = str;
      };
      profilesPath = mkOption {
        type = str;
      };
    };
  };
in {
  options.dotfield.paths = {
    userDirs = mkOption {
      type = lib.types.submodule userDirsSubmodule;
      readOnly = true;
    };
  };

  config.dotfield.paths = {
    userDirs = {
      basePath = "${fsPath}/home/users/${username}";
      configsPath = "${cfg.userDirs.basePath}/config";
      profilesPath = "${cfg.userDirs.basePath}/profiles";
    };
  };
}
