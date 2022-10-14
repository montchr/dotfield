moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  sysLib = moduleArgs.osConfig.lib.dotfield or {};
in {
  lib.dotfield = {
    features = rec {
      inherit (sysLib.sys) hasHidpi;
      hasPragPro = lib.strings.hasPrefix "PragmataPro" config.theme.fonts.mono.family;
      hasSway = config.wayland.windowManager.sway.enable;
      hasTwm = sysLib.sys.hasTwm or hasSway;
      hasWayland = sysLib.sys.hasWayland or hasSway;
    };
  };
}
