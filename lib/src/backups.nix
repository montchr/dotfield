# SPDX-FileCopyrightText: Copyright (c) 2023-2024 Chris Montgomery <chmont@proton.me>
# SPDX-FileCopyrightText: Copyright (c) 2022-2023 Gabriel Arazas <foodogsquared@foodogsquared.one>
# SPDX-License-Identifier: GPL-3.0-or-later AND MIT
#
## Sources:
#
# <https://github.com/foo-dogsquared/nixos-config/blob/7dacbe6963935fa3a36713c74c4e623df1d2188e/hosts/plover/default.nix>
{ flake, ... }:
let
  inherit (flake.inputs.nixpkgs) lib;
in
{
  mkJob =
    {
      keyFile,
      passCommand,
      paths,
      repo,
      patterns ? [ ],
      patternFiles ? [ ],
      startAt ? "daily",
      persistentTimer ? true,
    }:
    {
      inherit
        paths
        persistentTimer
        repo
        startAt
        ;

      # storage efficiency > speed
      compression = "zstd,11";
      dateFormat = "+%Y-%m-%dT%H:%M:%S";
      doInit = true; # set to false if repo is not consistently available e.g. network drive
      encryption = {
        inherit passCommand;
        mode = "repokey-blake2";
      };
      extraCreateArgs =
        let
          args = lib.flatten [
            (map (file: "--patterns-from ${lib.escapeShellArg file}") patternFiles)
            (map (pattern: "--pattern ${lib.escapeShellArg pattern}") patterns)
          ];
        in
        lib.concatStringsSep " " args;
      exclude = [
        "/nix"
        "'**/.cache'" # note the single quotes -- copied as is from NixOS manual
        "**/node_modules"
      ];
      extraInitArgs = "--make-parent-dirs";
      preHook = ''
        extraCreateArgs="$extraCreateArgs --verbose --stats --checkpoint-interval 600"
      '';
      prune.keep = {
        hourly = 2;
        daily = 5;
        weekly = 4;
        monthly = 12;
        yearly = 3;
      };
      environment."BORG_RSH" = "ssh -i ${keyFile}";
    };
}
