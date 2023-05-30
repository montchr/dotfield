moduleArgs @ {
  flake,
  config,
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
      moduleArgs.osConfig.fonts.fontconfig.defaultFonts
      or {
        monospace = ["DejaVu Sans Mono"];
        sansSerif = ["DejaVu Sans"];
        serif = ["DejaVu Serif"];
      };
  in
    l.mapAttrs (_: l.head) fonts;

  normalWeight = 400;

  colorSchemeModule = import ./__colorScheme.nix {inherit flake;};
in {
  options.theme = {
    enable = l.mkEnableOption "Whether to enable the theme module.";
    color.schemes = l.mkOption {
      type = with l.types; attrsOf (submodule colorSchemeModule);
      default = {
        default = mkColorScheme schemes.default-dark;
        dark = mkColorScheme schemes.default-dark;
        light = mkColorScheme schemes.default-light;
      };
    };
    fonts = {
      monospace = {
        name = mkOpt str defaultFonts.monospace;
        weight = mkOpt int normalWeight;
        size = mkOpt int 13;
      };
      terminal = with cfg.fonts; {
        name = mkOpt str monospace.family;
        weight = mkOpt int monospace.weight;
        size = mkOpt int monospace.size;
      };
      sansSerif = {
        name = mkOpt str defaultFonts.sansSerif;
        weight = mkOpt int normalWeight;
        size = mkOpt int 10;
      };
      serif = {
        name = mkOpt str defaultFonts.serif;
        weight = mkOpt int normalWeight;
        size = mkOpt int cfg.fonts.sansSerif.size;
      };
      emoji.name = mkOpt str "";
      symbols.name = mkOpt str "";
    };
  };
}
