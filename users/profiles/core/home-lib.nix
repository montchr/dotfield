moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.xdg) configHome dataHome stateHome;
  inherit (config.home) username;
in {
  lib.dotfield = rec {
    fsPath = "${configHome}/dotfield";
    userConfigPath = "${fsPath}/users/${username}/config";

    # FIXME: move this back to a module -- guardian
    whoami = {
      firstName = "Chris";
      lastName = "Montgomery";
      fullName = "${whoami.firstName} ${whoami.lastName}";
      email = "chris@cdom.io";
      githubUserName = "montchr";
      pgpPublicKey = "0x135EEDD0F71934F3";
    };

    emacs = {
      profilesBase = "emacs/profiles";
      profilesPath = "${userConfigPath}/${emacs.profilesBase}";
    };
  };
}
