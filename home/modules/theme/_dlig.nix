# FIXME: should be per-font, not global
{ config, lib, ... }:
let
  cfg = config.theme;
in
{
  options.theme = {
    fonts.features = {
      dlig.enable = lib.mkEnableOption "Whether to enable discretionary ligatures";
    };
  };
  config = lib.mkIf cfg.fonts.features.dlig.enable {

    programs.ghostty.settings.font-family = cfg.fonts.terminal.name;
    programs.ghostty.settings.font-feature = lib.optional cfg.fonts.dlig.enable [
      "-calt"
      "+dlig"
    ];

    programs.kitty.extraConfig =
      let
        inherit (cfg.fonts.terminal) psNamespace;
      in
      lib.optionalString cfg.fonts.dlig.enable ''
        # NOTE: Iosevka's "Regular" weight has no PostScript suffix
        font_features ${psNamespace}             -calt +dlig
        font_features ${psNamespace}-Bold        -calt +dlig
        font_features ${psNamespace}-Italic      -calt +dlig
        font_features ${psNamespace}-BoldItalic  -calt +dlig
      '';
  };
}
