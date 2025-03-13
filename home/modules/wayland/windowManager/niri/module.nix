{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    literalExpression
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    types
    ;
  cfg = config.wayland.windowManager.niri;
in
{
  options.wayland.windowManager.niri = {
    enable = mkEnableOption "the Niri window manager";

    configFile = mkOption {
      type = types.path;
      default = null;
      description = ''
        Path to the Niri configuration file.
      '';
    };

    package = mkOption {
      type = types.package;
      default = pkgs.niri;
      defaultText = literalExpression "pkgs.niri";
      description = "Niri package to use.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [ cfg.package ];
    }
    (mkIf (cfg.configFile != null) {
      xdg.configFile."niri/config.kdl" = {
        source = cfg.configFile;
      };
    })
  ]);
}
