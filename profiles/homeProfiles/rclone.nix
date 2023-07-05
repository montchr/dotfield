{
  flake,
  pkgs,
  ...
}: {
  home.packages = [
    # FIXME: <https://github.com/montchr/dotfield/issues/92>
    (
      if pkgs.stdenv.hostPlatform.isDarwin
      then flake.perSystem.inputs'.nixos-stable.legacyPackages.rclone
      else pkgs.rclone
    )
  ];
  home.sessionVariables = {
    NNN_RCLONE = "rclone mount --read-only --no-checksum --fast-list";
  };
}
