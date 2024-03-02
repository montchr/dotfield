# <https://github.com/NixOS/nix/issues/7273>
# <https://github.com/reckenrode/nixos-configs/blob/221fab620c8750a2fd506e9a3fb388c925fe322c/common/darwin/nix-optimizations-darwin.nix>
# <https://github.com/dxmh/system-config/commit/9713b5b39ae8c3394584e10132796df2dd497702>
{
  config,
  lib,
  pkgs,
  ...
}: {
  nix.distributedBuilds = lib.mkDefault true;

  # FIXME: needs flake-compat
  # nix.nixPath = mkBefore ["darwin-config=${self}"];

  nix.settings.auto-optimise-store = false;
  # While it’s possible to set `auto-optimise-store` in `nix.conf`, it sometimes causes problems
  # on Darwin.  Run a job periodically to optimise the store.
  launchd.daemons."nix-store-optimise".serviceConfig = {
    ProgramArguments = [
      "/bin/sh"
      "-c"
      ''
        /bin/wait4path ${config.nix.package}/bin/nix && \
          exec ${config.nix.package}/bin/nix store optimise
      ''
    ];
    StartCalendarInterval = [
      {
        Hour = 2;
        Minute = 0;
      }
    ];
    StandardErrorPath = "/var/log/nix-store.log";
    StandardOutPath = "/var/log/nix-store.log";
  };

  nix.gc = {
    automatic = true;
    interval = {
      Weekday = 0;
      Hour = 2;
      Minute = 0;
    };
    options = "--delete-older-than 30d";
  };
}
