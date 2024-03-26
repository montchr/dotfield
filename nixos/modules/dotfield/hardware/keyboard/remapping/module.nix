{ lib, config, ... }:
let
  cfg = config.dotfield.hardware.keyboard.remapping;
  hasProvider = s: cfg.provider == s;
in
{
  imports = [ ./options.nix ];

  config = lib.mkIf cfg.enable {
    services.kanata.enable = hasProvider "kanata";
    services.keyd.enable = hasProvider "keyd";
    services.kmonad.enable = hasProvider "kmonad";
  };
}
