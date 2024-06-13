{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf mkMerge;

  cfg = config.dotfield.activities.development;
in
{
  config = mkIf cfg.enable mkMerge [
    { home.shellAliases."j" = "just"; }
    (mkIf config.programs.emacs.enable {
      home.packages = [ pkgs.emacs-lsp-booster ];
      programs.emacs.defaultEditor.enable = true;
    })
  ];
}
