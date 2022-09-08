{self, ...}: let
  inherit (self.inputs.flake-utils.lib) filterPackages flattenTree;
in {
  perSystem = {
    pkgs,
    system,
    ...
  }: {
    packages =
      filterPackages system (flattenTree
        (import ./all-packages.nix pkgs));
  };
}
