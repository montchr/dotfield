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

  # perSystem =
  #   { ... }:
  #   {
  #     nix-unit.inputs = {
  #       inherit (inputs)
  #         nixpkgs
  #         nixpkgs-lib
  #         flake-parts
  #         git-hooks
  #         globset
  #         apparat
  #         nix-unit
  #         ;
  #     };
  #   };
}
