{
  lib,
  self,
  inputs,
  ...
}:
{
  imports =
    (inputs.globset.lib.globs ./. [
      "*/**/*.nix"
    ])
    |> lib.fileset.toList;

  perSystem =
    { ... }:
    {
      nix-unit.inputs = {
        inherit (inputs)
          nixpkgs
          nixpkgs-lib
          flake-parts
          apparat
          nix-unit
          ;
      };
    };
}
