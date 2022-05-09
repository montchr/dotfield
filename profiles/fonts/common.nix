{
  config,
  lib,
  pkgs,
  ...
}: {
  fonts = {
    fontDir.enable = true;
    fonts = with pkgs;
      [
        b612
        barlow
        fira
        ibm-plex
        inter
        jost
        public-sans
      ]
      ++ (lib.optionals stdenv.hostPlatform.isLinux [
        corefonts
        inconsolata
        liberation_ttf
        dejavu_fonts
        bakoma_ttf
        gentium
        ubuntu_font_family
        terminus_font
      ])
      ++ (lib.optionals stdenv.hostPlatform.isMacOS [
        sf-pro
      ]);
  };
}
