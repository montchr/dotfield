{
  self,
  config,
  flake-parts-lib,
  ...
}:
let
  inherit (flake-parts-lib) importApply;
  lib' = self.lib;
in
{
  flake.modules.homeManager = {
    bash-trampoline = ./programs/bash/trampoline/_module.nix;
    blesh = importApply ./programs/bash/blesh/_module.nix {
      inherit lib';
    };
    jujutsu-signing = importApply ./programs/jujutsu/signing/_module.nix {
      inherit (config) meta;
    };
    kanata = ./services/kanata/_module.nix;
  };
}
