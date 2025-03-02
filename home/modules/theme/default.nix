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
  inherit (apparat.lib.typography) fontWeights;
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
    color.scheme = {
      default = mkColorSchemeOption cfg.color.scheme.dark;
      dark = mkColorSchemeOption (mkColorScheme schemes.default-dark);
      light = mkColorSchemeOption (mkColorScheme schemes.default-light);
    };
    font = {
      monospace = {
        name = mkOpt str defaultFonts.monospace;
        weight = mkOpt int fontWeights.normal;
        size = mkOpt int 12;
        package = mkPackageOption "monospace";
        psNamespace = mkOpt str "";
      };
      terminal = {
        name = mkOpt str defaultFonts.monospace;
        weight = mkOpt int fontWeights.normal;
        size = mkOpt int 12;
        package = mkPackageOption "terminal";
        psNamespace = mkOpt str "";
      };
      sansSerif = {
        name = mkOpt str defaultFonts.sansSerif;
        weight = mkOpt int fontWeights.normal;
        size = mkOpt int 12;
        package = mkPackageOption "sans-serif";
        psNamespace = mkOpt str "";
      };
      serif = {
        name = mkOpt str defaultFonts.serif;
        weight = mkOpt int fontWeights.normal;
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
      colorScheme = cfg.color.scheme;
      sessionVariables = {
        #: current
        DOTFIELD_COLORS = colorScheme.default.name;
        DOTFIELD_THEME_MODE = colorScheme.default.kind;

        #: alternatives
        DOTFIELD_COLORS_DARK = colorScheme.dark.name;
        DOTFIELD_COLORS_LIGHT = colorScheme.light.name;
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
      #     theme.color.scheme.default = colorScheme.dark;
      #   };
      #   light.configuration = {
      #     theme.color.scheme.default = colorScheme.light;
      #   };
      # };

      fonts.fontconfig.enable = true;
      fonts.fontconfig.defaultFonts = {
        monospace = lib.mkBefore [ cfg.font.monospace.name ];
        sansSerif = lib.mkBefore [ cfg.font.sansSerif.name ];
        serif = lib.mkBefore [ cfg.font.serif.name ];
      };

      home.packages = [
        cfg.font.monospace.package
        cfg.font.sansSerif.package
        cfg.font.serif.package
      ];

      dconf.settings =
        let
          fontspecFor =
            font:
            lib.concatStringsSep " " [
              font.name
              (builtins.toString font.size)
            ];
          sans = fontspecFor cfg.font.sansSerif;
          serif = fontspecFor cfg.font.serif;
          mono = fontspecFor cfg.font.monospace;
        in
        {
          "org/gnome/desktop/interface" = {
            document-font-name = sans;
            font-name = sans; # default font
            color-scheme = "prefer-" + cfg.color.scheme.default.kind;
            monospace-font-name = mono;
          };
          "org/gnome/desktop/wm/preferences" = {
            titlebar-uses-system-font = true;
          };
        };
    }
  );
}
