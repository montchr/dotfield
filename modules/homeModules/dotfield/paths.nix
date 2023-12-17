# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
# TODO: https://github.com/ncfavier/config/blob/667516e2ea95a0f6604290f1c27f3bd79bc909bd/modules/nix.nix#L11-L15
{
  config,
  lib,
  ...
} @ moduleArgs: let
  inherit (lib) mkOption;
  inherit (lib.types) str;
  inherit (config.home) username;
  cfg = config.dotfield.paths;
  # FIXME: this will break in standalone configs.
  #        needs an `or`... but therein lies a rabbit hole... (why?)
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
      # FIXME: broken?!
      basePath = "${fsPath}/home/users/${username}";
      configsPath = "${cfg.userDirs.basePath}/config";
      profilesPath = "${cfg.userDirs.basePath}/profiles";
    };
  };
}
