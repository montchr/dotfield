{ inputs, ... }:
{
  imports = [ (inputs.import-tree ./lib) ];

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
