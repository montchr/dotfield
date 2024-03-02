moduleArgs @ {
  flake,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (flake.inputs) apparat base16-schemes;
  inherit (flake.self.lib.theme) mkColorScheme;
  inherit (apparat.lib) mkOpt;
  inherit (base16-schemes.lib) schemes;
  inherit (l.types) str int;

  l = flake.inputs.nixpkgs.lib // builtins;
  cfg = config.theme;

  # Single-item list format follows the NixOS options.
  defaultFonts = let
    fonts =
      # moduleArgs.osConfig.fonts.fontconfig.defaultFonts or
      {
        monospace = ["DejaVu Sans Mono"];
        sansSerif = ["DejaVu Sans"];
        serif = ["DejaVu Serif"];
      };
  in
    l.mapAttrs (_: l.head) fonts;

  # TODO: get this from apparat constant
  normalWeight = 400;

  colorSchemeModule = import ./__colorScheme.nix {inherit flake;};

  # TODO: add description and example
  mkColorSchemeOption = default:
    l.mkOption {
      inherit default;
      type = with l.types; (submodule colorSchemeModule);
    };

  mkPackageOption = type: l.mkPackageOption pkgs "${type} font" {default = null;};
in {
  options.theme = {
    enable = l.mkEnableOption "Whether to enable the theme module.";
    color.schemes = {
      default = mkColorSchemeOption cfg.color.schemes.dark;
      dark = mkColorSchemeOption (mkColorScheme schemes.default-dark);
      light = mkColorSchemeOption (mkColorScheme schemes.default-light);
    };
    fonts = {
      monospace = {
        name = mkOpt str defaultFonts.monospace;
        weight = mkOpt int normalWeight;
        size = mkOpt int 13;
        package = mkPackageOption "monospace";
        psNamespace = mkOpt str "";
      };
      terminal = with cfg.fonts; {
        name = mkOpt str monospace.name;
        weight = mkOpt int monospace.weight;
        size = mkOpt int monospace.size;
        package = mkPackageOption "terminal";
        psNamespace = mkOpt str "";
      };
      sansSerif = {
        name = mkOpt str defaultFonts.sansSerif;
        weight = mkOpt int normalWeight;
        size = mkOpt int 10;
        package = mkPackageOption "sans-serif";
        psNamespace = mkOpt str "";
      };
      serif = {
        name = mkOpt str defaultFonts.serif;
        weight = mkOpt int normalWeight;
        size = mkOpt int cfg.fonts.sansSerif.size;
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

  config = lib.mkIf cfg.enable (let
    colorSchemes = cfg.color.schemes;
    sessionVariables = {
      #: current
      DOTFIELD_COLORS = colorSchemes.default.name;
      DOTFIELD_THEME_MODE = colorSchemes.default.kind;

      #: alternatives
      DOTFIELD_COLORS_DARK = colorSchemes.dark.name;
      DOTFIELD_COLORS_LIGHT = colorSchemes.light.name;
    };
  in {
    home = {inherit sessionVariables;};
    programs.bash = {inherit sessionVariables;};
    programs.zsh = {inherit sessionVariables;};

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

    dconf.settings = {
      # TODO: other font styles?
      "org/gnome/desktop/interface" = let
        # TODO: add to lib
        fontname = font: lib.concatStringsSep " " [font.name (builtins.toString font.size)];
        sans = fontname cfg.fonts.sansSerif;
        serif = fontname cfg.fonts.serif;
        mono = fontname cfg.fonts.monospace;
      in {
        document-font-name = sans;
        color-scheme = lib.mkDefault "prefer-dark";
        monospace-font-name = mono;
      };
    };
  });
}
