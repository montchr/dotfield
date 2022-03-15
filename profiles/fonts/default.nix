{
  config,
  lib,
  pkgs,
  ...
}: {
  nixpkgs.overlays = [(import ../../overlays/internal/fonts.nix)];
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
