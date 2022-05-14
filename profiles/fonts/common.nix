{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux isMacOS;
  # inherit (config.theme) fonts;
in
  lib.mkMerge [
    {
      environment.systemPackages = with pkgs; [
        (lib.mkIf isLinux font-manager)
      ];

      fonts = {
        fontDir.enable = true;
        fonts = with pkgs; [
          b612
          barlow
          fira
          fira-code
          fira-code-symbols
          ibm-plex
          inter
          jost
          material-design-icons
          material-icons
          nerdfonts
          public-sans
        ];
      };
    }

    (lib.mkIf isLinux {
      fonts = {
        fonts = with pkgs; [
          corefonts
          inconsolata
          liberation_ttf
          dejavu_fonts
          bakoma_ttf
          gentium
          ubuntu_font_family
          terminus_font
        ];
        fontconfig = {
          enable = lib.mkForce true;
          defaultFonts = {
            monospace = ["PragmataPro Liga 16"];
            sansSerif = ["IBM Plex Sans 14"];
            serif = ["IBM Plex Serif 14"];
          };
          # defaultFonts = {
          #   monospace = [ "${fonts.mono.family} ${toString fonts.mono.size}" ];
          #   sansSerif = [ "${fonts.main.family} ${toString fonts.main.size}" ];
          #   serif = [ "${fonts.serif.family} ${toString fonts.serif.size}" ];
          # };
        };
        enableDefaultFonts = true;
      };
    })

    (lib.mkIf isMacOS {
      fonts.fonts = with pkgs; [
        sf-pro
      ];
    })
  ]
