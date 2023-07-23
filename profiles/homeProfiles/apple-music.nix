{
  flake,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isAarch64 isLinux;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  home.packages = l.optional (isLinux && !isAarch64) pkgs.cider;
}
