# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  self,
  config,
  lib,
  ...
}: let
  inherit (lib) mkOption;
  inherit (lib.types) str;
  inherit (config.home) username;
  userDirsSubmodule = {options, ...}: {
    options = {
      configs = mkOption {
        type = str;
      };
      profiles = mkOption {
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
    userDirs.configs = self + "/home/users/${username}/config";
    userDirs.profiles = self + "/home/users/${username}/profiles";
  };
}
