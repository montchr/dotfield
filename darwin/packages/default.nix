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
      kmonad-daemon-shim = callPackage ./kmonad/daemon-shim/package.nix {};
      kmonad-m2 = inputs'.kmonad.packages.kmonad.overrideAttrs (o: {
        patches = (o.patches or []) ++ [./kmonad/m2.patch];
      });
      yabai = callPackage ./yabai.nix {};
    });
  };
}
