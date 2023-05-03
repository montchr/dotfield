{pkgs, ...}: {
  environment.systemPackages = [pkgs.rclone];

  # FUSE-T for `rclone mount` support.
  # NOTE: <https://rclone.org/commands/rclone_mount/#fuse-t-limitations-caveats-and-notes>
  homebrew.taps = ["macos-fuse-t/homebrew-cask"];
  homebrew.casks = [
    "fuse-t"
    "fuse-t-sshfs"
  ];
}
