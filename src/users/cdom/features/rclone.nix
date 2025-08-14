{ lib, ... }:
{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    let
      # FIXME: not available...?
      rclone-start-mount =
        {
          readonly ? true,
          expire ? "168h", # 7 days
          mode ? "full", # or "writes"
        }:
        (pkgs.writeShellScriptBin "rclone-start-mount" ''
          ${pkgs.rclone}/bin/rclone ${
            lib.cli.toGNUCommandLineShell { } {
              vfs-read-chunk-size = "64M";
              vfs-read-chunk-size-limit = "2048M";
              vfs-cache-mode = mode;
              buffer-size = "128M";
              max-read-ahead = "256M";
              dir-cache-time = expire;
              timeout = "10m";
              transfers = 16;
              checkers = 12;
              drive-chunk-size = "64M";
              # fuse-flag = ["sync_read" "auto_cache"];
              read-only = readonly;
              umask = "002";
              verbose = true;
            }
          } mount ''${@}
        '');
    in
    {
      home.packages = [
        pkgs.rclone
        (rclone-start-mount { })
      ];
      home.sessionVariables = {
        NNN_RCLONE = "rclone mount --fast-list";
      };
    };
}
