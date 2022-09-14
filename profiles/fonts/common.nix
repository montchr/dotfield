{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux isMacOS;
in {
  imports = [./iosevka-variants.nix];

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

        (nerdfonts.override {
          fonts = [
            "Iosevka"
            "NerdFontsSymbolsOnly"
          ];
        })

        iosevka-xtal.xtal
        iosevka-xtal.xtal-term

        # "Iosevka Comfy" by Protesilaos Stavrou
        # https://git.sr.ht/~protesilaos/iosevka-comfy
        # https://protesilaos.com/emacs/iosevka-comfy-pictures
        iosevka-comfy.comfy
        iosevka-comfy.comfy-duo
        iosevka-comfy.comfy-wide
        iosevka-comfy.comfy-wide-fixed
      ]
      # TODO: all systems
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
