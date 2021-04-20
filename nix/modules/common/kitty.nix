{ pkgs, lib, config, options, ... }:

let
  cfg = config.my.modules.kitty;
in
{
  options = with lib; {
    my.modules.kitty = {
      enable = mkEnableOption ''
        Whether to enable kitty module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable (mkMerge [
      # TODO: there's probably a better way to handle this
      (if (builtins.hasAttr "homebrew" options) then {
        homebrew.casks = [ "kitty" ];
      } else {
        my.user = { packages = with pkgs; [ kitty ]; };
      })

      {
        my.env = {
          TERMINFO_DIRS = "${pkgs.kitty.terminfo.outPath}/share/terminfo";
        };

        my.hm.file = {
          ".config/kitty" = {
            recursive = true;
            source = ../../../config/kitty;
          };
        };
      }
    ]);
}
