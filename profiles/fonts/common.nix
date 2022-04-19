{
  config,
  lib,
  pkgs,
  ...
}: {
  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      b612
      barlow
      fira
      ibm-plex
      inter
      jost
      public-sans
      recursive
      sf-pro
    ];
  };
}
