{
  lib,
  self,
  withSystem,
  ops,
  ...
}:
let
  inherit (self.inputs) apparat haumea;

  lib' = haumea.lib.load {
    src = ./src;
    inputs = {
      inherit apparat lib ops;
    };
  };
in
{
  flake.lib = lib' // {
    # FIXME: remove
    # inherit ops;

    specialArgsFor =
      system:
      withSystem system (
        {
          inputs',
          config,
          pkgs,
          ...
        }:
        {
          # FIXME: remove
          inherit ops;

          flake = {
            inherit (self) inputs;
            inherit inputs' ops;
            lib = lib';
            inherit (config) packages;
            path = self.outPath;
          };
        }
      );
  };
}
