{
  config,
  lib,
  pkgs,
  ...
}: {
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      b612
      barlow
      fira
      ibm-plex
      inter
      jost
      pragmatapro
      public-sans
      recursive
      sf-pro
    ];
  };
}
