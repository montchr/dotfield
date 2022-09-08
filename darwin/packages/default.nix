{self, ...}: let
  inherit (self.inputs.flake-utils.lib) filterPackages flattenTree;
in {
  perSystem = {system, ...}: {
    packages = filterPackages system (flattenTree {
      # emacs-plus = pkgs.callPackage ./emacs-plus.nix {};
      # yabai = pkgs.callPackage ./yabai.nix {};
      yabai = ./yabai.nix;
    });
  };
}
