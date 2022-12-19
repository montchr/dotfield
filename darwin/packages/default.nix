{inputs, ...}: let
  inherit (inputs.flake-utils.lib) filterPackages flattenTree;
in {
  perSystem = {
    system,
    pkgs,
    inputs',
    ...
  }: let
    inherit (pkgs) callPackage;
    inherit (inputs'.emacs-overlay.packages) emacsGit emacsUnstable;
  in {
    packages = filterPackages system (flattenTree {
      emacs-plus = callPackage ./emacs-plus.nix {inherit emacsUnstable;};
      emacs-plus-edge = callPackage ./emacs-plus-edge.nix {inherit emacsGit;};
      yabai = callPackage ./yabai.nix {};
    });
  };
}
