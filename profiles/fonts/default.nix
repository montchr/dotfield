{
  config,
  lib,
  pkgs,
  ...
}: {
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      ibm-plex
      inter
      pragmatapro
      public-sans
    ];
  };
}
