{inputs, ...}: {
  perSystem = {
    inputs',
    packages,
    pkgs,
    ...
  }: {
    packages.devenv = inputs'.devenv.packages.devenv;
    devShells.dotfield-devenv = inputs.devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        {_module.args = {inherit inputs' packages;};}
        ./profiles/dotfield.nix
      ];
    };
  };
}
