{
  pkgs,
  flake,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isAarch64 isLinux;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  home.packages = l.optionals (isLinux && !isAarch64) [
    pkgs.spotify
    pkgs.spotify-tui
  ];
}
