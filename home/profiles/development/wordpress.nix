{
  config,
  lib,
  pkgs,
  profiles,
  ...
}: let
  gitCmd = "${pkgs.git}/bin/git";
in {
  home.packages = with pkgs; [
    trellis-cli

    # FIXME: assumes a particular repo structure -- likely incompatible with trellis
    (writeShellScriptBin "wp-debug-display" ''
      REPO_ROOT="$(${gitCmd} rev-parse --show-toplevel)"
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
      "${gnused}/bin/sed" \
        -i "/WP_DEBUG_DISPLAY/s/''${CURRENT_STATE}/''${NEW_STATE}/" \
        "''${REPO_ROOT}/../wp-config.php"
    '')
  ];

  home.shellAliases = {
    "trellis" = "${pkgs.trellis-cli}/bin/trellis-cli";
  };
}
