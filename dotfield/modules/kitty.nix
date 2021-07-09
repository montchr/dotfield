{ pkgs, lib, config, options, ... }:

let
  cfg = config.my.modules.kitty;
  configDir = "${config.dotfield.configDir}/kitty";

  scripts = with pkgs; {
    kittyGetWindow = (writeShellScriptBin "kitty-get-window-by-platform-id" ''
      kitty @ --to $KITTY_SOCKET ls \
        | ${pkgs.jq}/bin/jq -r --argjson id "$1" \
          '.[] | select(.platform_window_id==$id)'
    '');
  };
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
      {
        my.user.packages = (map (key: getAttr key scripts) (attrNames scripts));
        environment.variables = {
          TERMINFO_DIRS = "${pkgs.kitty.terminfo.outPath}/share/terminfo";
        };
      }
    ]);
}
