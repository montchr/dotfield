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
        ###: --- essentials ---

        corefonts
        dejavu_fonts
        inconsolata
        liberation_ttf
        terminus_font
        ubuntu_font_family

        ###: --- preferred defaults ---

        ibm-plex
        iosevka-xtal.xtal
        iosevka-xtal.xtal-term

        ###: --- optionals ---

        b612
        barlow
        emacs-all-the-icons-fonts
        fira
        inter
        jost
        public-sans

        (nerdfonts.override {
          fonts = [
            "Iosevka"
            "NerdFontsSymbolsOnly"
          ];
        })

        # "Iosevka Comfy" by Protesilaos Stavrou
        # https://git.sr.ht/~protesilaos/iosevka-comfy
        # https://protesilaos.com/emacs/iosevka-comfy-pictures
        iosevka-comfy.comfy
        iosevka-comfy.comfy-duo
        iosevka-comfy.comfy-wide
        iosevka-comfy.comfy-wide-fixed
      ]
      ++ (optionals isLinux [
        bakoma_ttf
        gentium
      ])
      ++ (optionals isMacOS [
        sf-pro
      ]);
  };
}
