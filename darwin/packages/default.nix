{
  self,
  withSystem,
  lib,
  ...
}: let
  inherit (self.lib.digga) flattenTree rakeLeaves;
  packageSources = flattenTree (rakeLeaves ./darwin/packages);
in {
  perDarwinSystem = {pkgs, ...}: {
    packages = {
      emacs-plus = pkgs.callPackage ./emacs-plus.nix {};
      yabai = pkgs.callPackage ./yabai.nix {};
    };
  };
}
