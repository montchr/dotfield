{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkDefault;
in {
  environment.systemPackages = with pkgs; [
    font-manager
  ];

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = mkDefault ["Iosevka Xtal"];
      serif = mkDefault ["IBM Plex Serif"];
      sansSerif = mkDefault ["IBM Plex Sans"];
    };
  };
}
