{self, ...}: let
  inherit (self) inputs;
  l = inputs.nixpkgs.lib // builtins;
in {
  perSystem = {
    inputs',
    pkgs,
    ...
  }: let
    inherit (inputs'.devshell.legacyPackages) mkShell;
  in {
    devShells = l.mapAttrs (_: v: mkShell (import v {inherit inputs' pkgs;})) {
      dotfield = ./dotfield.nix;
      ci = ./ci.nix;
    };
  };
}
