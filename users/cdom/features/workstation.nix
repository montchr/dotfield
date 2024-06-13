{ config, lib, ... }:
let
  cfg = config.dotfield;
in
{
  options.dotfield.features = {
    workstation = {
      enable = lib.mkEnableOption "workstation";
    };
  };
  config = lib.mkIf cfg.features.workstation.enable {
    dotfield.activities.writing.enable = true;

    services.git-sync.enable = true;

    programs.emacs.defaultEditor.enable = true;
    programs.emacs.extraPackages = epkgs: [
      epkgs.treesit-grammars.with-all-grammars
      epkgs.treesit-auto
    ];
  };
}
