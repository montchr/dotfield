{ config, flake-parts-lib, ... }:
let
  inherit (flake-parts-lib) importApply;
in
{
  flake.modules.home = {
    "jujutsu/signing" = importApply ./__signing.nix {
      inherit (config) meta;
    };
  };
}
