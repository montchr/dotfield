# FIXME: don't use config values as defaults for options! it does not make
# sense, and will likely cause issues in non-ideal circumstances.
moduleArgs@{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (builtins) head mapAttrs;
  inherit (lib.types) bool str int;
  inherit (flake.inputs) apparat base16-schemes;
  inherit (flake.self.lib.theme) mkColorScheme;
  inherit (apparat.lib) mkOpt;
  inherit (base16-schemes.lib) schemes;

  cfg = config.theme;

  # Single-item list format follows the NixOS options.
  defaultFonts =
    let
      fonts = {
        monospace = [ "DejaVu Sans Mono" ];
        sansSerif = [ "DejaVu Sans" ];
        serif = [ "DejaVu Serif" ];
      };
    in
    mapAttrs (_: head) fonts;

  # TODO: get this from apparat constant
  normalWeight = 400;

  colorSchemeModule = import ./__colorScheme.nix { inherit flake; };

  # TODO: add description and example
  mkColorSchemeOption =
    default:
    lib.mkOption {
      inherit default;
      type = with lib.types; (submodule colorSchemeModule);
    };

  mkPackageOption = type: lib.mkPackageOption pkgs "${type} font" { default = null; };
in
{
  imports = [ ./_dlig.nix ];

  options.theme = {
    enable = lib.mkEnableOption "Whether to enable the theme module.";
    color.schemes = {
      default = mkColorSchemeOption cfg.color.schemes.dark;
      dark = mkColorSchemeOption (mkColorScheme schemes.default-dark);
      light = mkColorSchemeOption (mkColorScheme schemes.default-light);
    };
    fonts = {
      monospace = {
        name = mkOpt str defaultFonts.monospace;
        weight = mkOpt int normalWeight;
        size = mkOpt int 12;
        package = mkPackageOption "monospace";
        psNamespace = mkOpt str "";
      };
      terminal = {
        name = mkOpt str defaultFonts.monospace;
        weight = mkOpt int normalWeight;
        size = mkOpt int 12;
        package = mkPackageOption "terminal";
        psNamespace = mkOpt str "";
      };
      sansSerif = {
        name = mkOpt str defaultFonts.sansSerif;
        weight = mkOpt int normalWeight;
        size = mkOpt int 12;
        package = mkPackageOption "sans-serif";
        psNamespace = mkOpt str "";
      };
      serif = {
        name = mkOpt str defaultFonts.serif;
        weight = mkOpt int normalWeight;
        size = mkOpt int 12;
        package = mkPackageOption "serif";
        psNamespace = mkOpt str "";
      };
      emoji = {
        name = mkOpt str "";
        package = mkPackageOption "emoji";
        psNamespace = mkOpt str "";
      };
      symbols = {
        name = mkOpt str "";
        package = mkPackageOption "symbols";
        psNamespace = mkOpt str "";
      };
    };
  };

  config = lib.mkIf cfg.enable (
    let
      colorSchemes = cfg.color.schemes;
      sessionVariables = {
        #: current
        DOTFIELD_COLORS = colorSchemes.default.name;
        DOTFIELD_THEME_MODE = colorSchemes.default.kind;

        #: alternatives
        DOTFIELD_COLORS_DARK = colorSchemes.dark.name;
        DOTFIELD_COLORS_LIGHT = colorSchemes.light.name;
      };
    in
    {
      home = {
        inherit sessionVariables;
      };
      programs.bash = {
        inherit sessionVariables;
      };
      programs.zsh = {
        inherit sessionVariables;
      };

      # FIXME: do themes some other way -- this barely works, and it makes all
      # builds take 5ever
      # specialisation = {
      #   dark.configuration = {
      #     theme.color.schemes.default = colorSchemes.dark;
      #   };
      #   light.configuration = {
      #     theme.color.schemes.default = colorSchemes.light;
      #   };
      # };

      fonts.fontconfig.enable = true;
      fonts.fontconfig.defaultFonts = {
        monospace = lib.mkBefore [ cfg.fonts.monospace.name ];
        sansSerif = lib.mkBefore [ cfg.fonts.sansSerif.name ];
        serif = lib.mkBefore [ cfg.fonts.serif.name ];
      };

      home.packages = [
        cfg.fonts.monospace.package
        cfg.fonts.sansSerif.package
        cfg.fonts.serif.package
      ];

      dconf.settings =
        let
          fontspecFor =
            font:
            lib.concatStringsSep " " [
              font.name
              (builtins.toString font.size)
            ];
          sans = fontspecFor cfg.fonts.sansSerif;
          serif = fontspecFor cfg.fonts.serif;
          mono = fontspecFor cfg.fonts.monospace;
        in
        {
          "org/gnome/desktop/interface" = {
            document-font-name = sans;
            font-name = sans; # default font
            color-scheme = "prefer-" + cfg.color.schemes.default.kind;
            monospace-font-name = mono;
          };
          "org/gnome/desktop/wm/preferences" = {
            titlebar-uses-system-font = true;
          };
        };
    }
  );
}
