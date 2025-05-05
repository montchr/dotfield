{ inputs }:
final: prev:
let
  stablePkgs = import inputs.nixos-stable {
    inherit (final) system;
    config.allowUnfree = true;
  };
in
{
  inherit (stablePkgs) calibre;
}
