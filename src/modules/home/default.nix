{ config, flake-parts-lib, ... }:
let
  inherit (flake-parts-lib) importApply;
in
{
  flake.modules.homeManager = {
    "programs/bash/trampoline" = ./programs/bash/trampoline/_module.nix;
    "programs/jujutsu/signing" = importApply ./programs/jujutsu/signing/_module.nix {
      inherit (config) meta;
    };
  };
}
