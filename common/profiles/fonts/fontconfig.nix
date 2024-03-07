{ lib, pkgs, ... }:
let
  inherit (lib) mkDefault;
in
{
  environment.systemPackages = with pkgs; [ font-manager ];

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = mkDefault [ "Iosevka" ];
      serif = mkDefault [ "IBM Plex Serif" ];
      sansSerif = mkDefault [ "Inter" ];
    };
  };
}
