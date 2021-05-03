{ pkgs, lib, config, options, ... }:

let

  cfg = config.my.modules.kitty;
  # cfgDir = "${config.dotfield.configDir}/kitty";

in {
  options = with lib; {
    my.modules.kitty = {
      enable = mkEnableOption ''
        Whether to enable kitty module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable (mkMerge [
      (if (builtins.hasAttr "homebrew" options) then {
        homebrew.casks = [ "kitty" ];
      } else {
        my.user = { packages = with pkgs; [ kitty ]; };
      })

      {
        my.env = {
          TERMINFO_DIRS = "${pkgs.kitty.terminfo.outPath}/share/terminfo";
        };

        # my.hm.configFile = {
        #   "kitty" = {
        #     recursive = true;
        #     source = cfgDir;
        #   };
        # };
      }
    ]);
}
