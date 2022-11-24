{self, ...}: let
  inherit  (self) inputs;
  l = inputs.nixpkgs.lib // builtins;
in {
  perSystem = {inputs', pkgs, ...} @ ctx: {
    devShells = l.mapAttrs (_: v: inputs'.devshell.legacyPackages.mkShell (import v (ctx // {inherit pkgs;}))) {
      dotfield = ./dotfield.nix;
      ci = ./ci.nix;
    };
  };
}
