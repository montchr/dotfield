{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  inherit (pkgs) system writeShellScriptBin;
  inherit (pkgs.lib.our) dotfieldPath;
in {
  home.packages = with pkgs; [
    php80
    php80Packages.composer

    (writeShellScriptBin "wp-debug-display" ''
      REPO_ROOT="$(git rev-parse --show-toplevel)"
      case "$1" in
        on|true)
          CURRENT_STATE="false"
          NEW_STATE="true"
          ;;
        off|false)
          CURRENT_STATE="true"
          NEW_STATE="false"
          ;;
      esac
      "${pkgs.gnused}/bin/sed" \
        -i "/WP_DEBUG_DISPLAY/s/''${CURRENT_STATE}/''${NEW_STATE}/" \
        "''${REPO_ROOT}/../wp-config.php"
    '')
  ];
  home.sessionPath = [
    # TODO: prepend not append
    "${dotfieldPath}/.composer/bin"
  ];
  home.sessionVariables = {
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
  };
}
