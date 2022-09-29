{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (lib) mkDefault mkIf;
  hasHidpi = config.hardware.video.hidpi.enable or false;
in (mkIf isLinux {
  environment.systemPackages = with pkgs; [
    font-manager
  ];

  fonts.fontconfig = {
    enable = true;
    antialias = !hasHidpi;
    subpixel.lcdfilter =
      if hasHidpi
      then "none"
      else "default";
    hinting.enable = !hasHidpi;
    defaultFonts = {
      monospace = mkDefault ["Iosevka Xtal"];
      serif = mkDefault ["IBM Plex Serif"];
      sansSerif = mkDefault ["IBM Plex Sans"];
    };
  };
})
