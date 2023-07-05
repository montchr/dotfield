{
  flake,
  # pkgs,
  ...
}: {
  # FIXME: <https://github.com/montchr/dotfield/issues/92>
  # environment.systemPackages = [pkgs.rclone];
  environment.systemPackages = [flake.perSystem.inputs'.nixos-stable.legacyPackages.rclone];

  # FUSE-T for `rclone mount` support.
  # NOTE: <https://rclone.org/commands/rclone_mount/#fuse-t-limitations-caveats-and-notes>
  homebrew.taps = ["macos-fuse-t/homebrew-cask"];
  homebrew.casks = [
    "fuse-t"
    "fuse-t-sshfs"
  ];
}
