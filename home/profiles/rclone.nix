{pkgs, ...}: {
  home.packages = [pkgs.rclone];
  home.sessionVariables = {
    NNN_RCLONE = "rclone mount --read-only --no-checksum --fast-list";
  };
}
