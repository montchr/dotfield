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
        inherit (inputs) nixpkgs flake-parts nix-unit;
      };
    };
}
