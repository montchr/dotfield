# SPDX-FileCopyrightText: Copyright (c) 2023 Chris Montgomery <chris@cdom.io>
# SPDX-FileCopyrightText: Copyright (c) 2022-2023 Gabriel Arazas <foodogsquared@foodogsquared.one>
# SPDX-License-Identifier: GPL-3.0-or-later OR MIT
### Sources:
# - <https://github.com/foo-dogsquared/nixos-config/blob/7dacbe6963935fa3a36713c74c4e623df1d2188e/hosts/plover/default.nix>
{ lib, config, ... }:
let
  inherit (config.sops) secrets;
  mkJob =
    {
      passCommand,
      paths,
      repo,
      patterns ? [ ],
      patternFiles ? [ ],
    }:
    {
      inherit paths repo;
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
      ];
      extraInitArgs = "--make-parent-dirs";
      persistentTimer = true;
      preHook = ''
        extraCreateArgs="$extraCreateArgs --stats"
      '';
      prune.keep = {
        daily = 5;
        weekly = 4;
        monthly = 12;
        yearly = 3;
      };
      startAt = "daily";
      environment."BORG_RSH" = "ssh -i ${secrets."borg/ssh-key".path}";
    };
in
{
  services.borgbackup.jobs = {
    host-backup = mkJob {
      paths = [ "/home" ];
      passCommand = "cat ${secrets."borg/repos/host/passphrase".path}";
      repo = "ssh://c7bv8x8g@c7bv8x8g.repo.borgbase.com/./repo";
    };
    services-backup = mkJob {
      # TODO: probably others
      paths = [ "/var/lib/acme" ];
      passCommand = "cat ${secrets."borg/repos/services/passphrase".path}";
      repo = "ssh://t1dij3bh@t1dij3bh.repo.borgbase.com/./repo";
    };
  };

  sops.secrets."borg/repos/host/passphrase" = { };
  sops.secrets."borg/repos/services/passphrase" = { };
  sops.secrets."borg/ssh-key" = { };
}
