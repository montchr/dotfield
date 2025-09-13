{ config, flake-parts-lib, ... }:
let
  inherit (flake-parts-lib) importApply;
in
{
  flake.modules.homeManager = {
    bash-trampoline = ./programs/bash/trampoline/_module.nix;
    jujutsu-signing = importApply ./programs/jujutsu/signing/_module.nix {
      inherit (config) meta;
    };
    kanata = ./services/kanata/_module.nix;
  };
}
