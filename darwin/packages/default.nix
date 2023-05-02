{inputs, ...}: let
  inherit (inputs.apparat.lib) tree;
  inherit (inputs.flake-utils.lib) filterPackages;
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
    packages = filterPackages system (tree.flatten {
      emacs-plus = callPackage ./emacs-plus.nix {inherit emacsUnstable;};
      emacs-plus-edge = callPackage ./emacs-plus-edge.nix {inherit emacsGit;};
    });
  };
}
