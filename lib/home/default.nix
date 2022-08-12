moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.xdg) configHome dataHome stateHome;
  inherit (config.home) username;

  sysLib = moduleArgs.osConfig.lib.dotfield or {};

  this = config.lib.dotfield;
in {
  lib.dotfield = {
    fsPath = "${configHome}/dotfield";
    userConfigPath = "${this.fsPath}/home/users/${username}/config";

    features = rec {
      hasPragPro = lib.strings.hasPrefix "PragmataPro" config.theme.font.mono.family;
      hasSway = config.wayland.windowManager.sway.enable;
      hasTwm = sysLib.sys.hasTwm or hasSway;
      hasWayland = sysLib.sys.hasWayland or hasSway;
    };

    # FIXME: move this back to a module -- guardian
    whoami = rec {
      firstName = "Chris";
      lastName = "Montgomery";
      fullName = "${firstName} ${lastName}";
      email = "chris@cdom.io";
      githubUserName = "montchr";
      pgpPublicKey = "0x135EEDD0F71934F3";
    };

    emacs = rec {
      profilesBase = "emacs/profiles";
      profilesPath = "${this.userConfigPath}/${profilesBase}";
    };
  };
}
