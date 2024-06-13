{
  lib,
  flake,
  pkgs,
  config,
  ...
}:
let
  cfg = config.dotfield.activities.writing;
in
{
  options.dotfield.activities.writing = {
    enable = lib.mkEnableOption "writing";
  };

  config = lib.mkIf cfg.enable {

    home.packages = [
      flake.packages.aspell-with-dicts

      pkgs.enchant
      pkgs.languagetool
    ];

    # Not the greatest workaround, but...
    # <https://github.com/minad/jinx/discussions/173#discussioncomment-9416580>
    home.sessionVariables.ASPELL_CONF = "dict-dir ${flake.packages.aspell-with-dicts}/lib/aspell";

    programs.emacs.extraPackages = epkgs: [
      (epkgs.jinx.override { enchant2 = pkgs.enchant; })
    ];
  };
}
