{ config, lib, pkgs, ... }:
let
  inherit (config.home) username;
  inherit
    (config.xdg)
    configHome
    dataHome
    stateHome
    ;

  hmLib = config.lib;

  doomRepoUrl = "https://github.com/doomemacs/doomemacs";
  profilePath = "emacs/profiles/doom";
  doomDataDir = "${dataHome}/${profilePath}";
  doomStateDir = "${stateHome}/${profilePath}";
in
{
  imports = [ ../common.nix ];

  home.sessionPath = [ "${configHome}/emacs/bin" "$PATH" ];
  home.sessionVariables = {
    # local state :: built files, dependencies, etc.
    DOOMLOCALDIR = doomStateDir;
  };

  xdg.configFile."doom".source = hmLib.file.mkOutOfStoreSymlink
    "${configHome}/dotfield/users/${username}/config/${profilePath}";

  # Note that the doom directory will need to be removed manually upon changing
  # profiles. home-manager will likely warn about this when activating another
  # profile though.
  home.activation.installDoomEmacs = hmLib.dag.entryAfter [ "writeBoundary" ] ''
    if [[ ! -d "$XDG_CONFIG_HOME/emacs" ]]; then
      $DRY_RUN_CMD ${pkgs.git}/bin/git clone \
        --depth=1 --single-branch "${doomRepoUrl}" \
        "${configHome}/emacs"
    fi
  '';
}
