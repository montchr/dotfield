{self, ...}: let
  inherit (self.inputs.flake-utils.lib) filterPackages flattenTree;
in {
  perSystem = {
    system,
    pkgs,
    ...
  }: {
    packages = filterPackages system (flattenTree {
      emacs-plus = pkgs.callPackage ./emacs-plus.nix {};
      emacs-plus-edge = pkgs.callPackage ./emacs-plus-edge.nix {};
      yabai = pkgs.callPackage ./yabai.nix {};
    });
  };
}
