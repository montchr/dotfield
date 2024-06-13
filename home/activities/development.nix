{
  flake,
  lib,
  pkgs,
  config,
  ...
}:
let
  inherit (lib) mkEnableOption mkIf mkMerge;
  inherit (flake.inputs') nil-lsp;
  cfg = config.dotfield.activities.development;
in
{
  options.dotfield.activities.development = {
    enable = mkEnableOption "development";
  };

  config = mkIf cfg.enable mkMerge [
    {
      home.packages = [
        nil-lsp.packages.nil

        pkgs.just
        pkgs.nixd
      ];
    }
    (mkIf config.programs.emacs.enable {
      home.packages = [ pkgs.emacs-lsp-booster ];
      programs.emacs.defaultEditor.enable = true;
    })
  ];
}
