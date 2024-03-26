{ lib, ... }:
let
  inherit (lib) mkEnableOption mkOption types;
in
{
  options.dotfield.hardware.keyboard.remapping = {
    enable = mkEnableOption "keyboard remapping";
    provider = mkOption {
      description = "Provider service handling keyboard event remapping.";
      default = "keyd";
      type = types.enum [
        "gnome"
        "kanata"
        "keyd"
        "kmonad"
      ];
    };
  };
}
