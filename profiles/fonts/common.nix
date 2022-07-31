{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux isMacOS;
in {
  environment.systemPackages = with pkgs; [
    (lib.mkIf isLinux font-manager)
  ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs;
      [
        b612
        barlow
        emacs-all-the-icons-fonts
        fira
        ibm-plex
        inter
        jost
        public-sans

        iosevka-seadome
        iosevka-xtal

        iosevka-nf
        iosevka-fixed
        iosevka-term

        iosevka-slab
        iosevka-fixed-slab
        iosevka-term-slab

        iosevka-curly
        iosevka-fixed-curly
        iosevka-term-curly

        iosevka-curly-slab
        iosevka-fixed-curly-slab
        iosevka-term-curly-slab

        iosevka-aile
        iosevka-etoile

        iosevka-comfy.comfy
        iosevka-comfy.comfy-duo
        iosevka-comfy.comfy-wide
        iosevka-comfy.comfy-wide-fixed
      ]
      ++ (lib.optionals isLinux [
        corefonts
        inconsolata
        liberation_ttf
        dejavu_fonts
        bakoma_ttf
        gentium
        ubuntu_font_family
        terminus_font
      ])
      ++ (lib.optionals isMacOS [
        sf-pro
      ]);
  };
}
