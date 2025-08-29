{ inputs, lib, ... }:
let
  eval =
    {
      configuration,
      lib ? inputs.nixpkgs-lib,
      extraSpecialArgs ? { },
    }:
    let
      module = lib.evalModules (
        import ./src/eval-args.nix {
          inherit lib extraSpecialArgs;
          modules = [ configuration ];
        }
      );
    in
    {
      inherit (module) config options;
      inherit (module.config.devshell) shell;
    };

  umwelt-lib = {

    mkShell = configuration: (eval { inherit configuration; }).shell;
  };
in
{
  options.umwelt = {

  };
}
