{self, ...}: let
  inherit (self) inputs;
  l = inputs.nixpkgs.lib // builtins;
in {
  perSystem = {
    inputs',
    pkgs,
    ...
  } @ ctx: let
    inherit (inputs'.devshell.legacyPackages) mkShell;
  in {
    devShells = l.mapAttrs (_: v: mkShell (import v (ctx // {inherit pkgs;}))) {
      dotfield = ./dotfield.nix;
      ci = ./ci.nix;
    };
  };
}
