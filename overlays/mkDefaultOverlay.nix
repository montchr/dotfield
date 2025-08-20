{ inputs }:
final: prev:
let
  stablePkgs = import inputs.nixos-stable {
    inherit (final) system;
    config.allowUnfree = true;
  };
  trunkPkgs = import inputs.nixpkgs-trunk {
    inherit (final) system;
    config.allowUnfree = true;
  };
in
{
  inherit (trunkPkgs) zellij;
}
