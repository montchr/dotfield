{
  config,
  lib,
  pkgs,
  isDarwin,
  isLinux,
  isMacOS,
  ...
}: let
  inherit (lib) optionals;
in {
  imports = [./iosevka-variants.nix];
  fonts = {
    fontDir.enable = true;
    fonts = with pkgs;
      [
        ###: --- essentials ---

        dejavu_fonts # dejavu_fonts
        inconsolata # i HeArD YoU lIkEd pr0gRAmMiNg F0nT5
        liberation_ttf # freedom as in freedom fries
        terminus_font # we are the robots
        ubuntu_font_family # ubuntu means fun!

        ibm-plex # ibm sponsors my media server

        # TODO: move to a separate flake repo w/its own build ci
        # FIXME: do not rebuild every time a build input changes! they're just
        # fonts! also causes a totally unnecessary new build for darwin...
        iosevka-xtal.xtal
        iosevka-xtal.xtal-term

        ###: --- optionals ---

        b612 # preparing for my pilot's license
        barlow # only here to remind me of sebastopol
        emacs-all-the-icons-fonts # because!
        fira # i wouldn't be a programmer without this font
        inter # fun fun fun on the autozone
        jost # if colin jost was a font?
        public-sans # slightly more boring than ibm-plex

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
        corefonts # broken on aarch64-darwin
        gentium
      ])
      ++ (optionals isMacOS [
        sf-pro
      ]);
  };
}
