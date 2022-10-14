# FIXME: remove after https://github.com/NixOS/nixpkgs/pull/193589
{self, ...}: let
  inherit (self) packages;
in {
  perSystem = {pkgs, ...}: {
    packages.jemalloc = pkgs.callPackage ./development/libraries/jemalloc {};
  };
  flake.overlays.nixpkgs-193589-jemalloc = final: prev: {
    inherit (packages.${final.system}) jemalloc;
  };
}
