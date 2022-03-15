{
  config,
  lib,
  pkgs,
  ...
}: {
  # Unavailable in nixpkgs.
  homebrew.brews = [
    # :lang org (macOS only)
    "pngpaste"
  ];
}
