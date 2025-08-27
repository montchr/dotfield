{
  flake-parts-lib,
  self,
  withSystem,
  ...
}:
let
  inherit (flake-parts-lib) importApply;
in
{
  flake.homeModules."bash/fzf-tab-completion" = importApply ./__module.nix {
    inherit withSystem;
    localFlake = self;
  };
}
