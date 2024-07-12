{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (flake.inputs) lix-module;
  cfg = config.lix;
in
{
  options.lix = {
    enable = lib.mkEnableOption "Lix";
  };

  config = lib.mkIf cfg.enable { nixpkgs.overlays = [ lix-module.overlays.default ]; };
}
