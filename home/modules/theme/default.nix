moduleArgs @ {
  config,
  inputs,
  self,
  ...
}: let
  inherit (l.types) nullOr str int;
  # inherit (inputs) apparat nix-colors;
  inherit (self.lib.colors) getColorScheme;

  # colorSchemeModule = apparat.homeManagerModules.colorScheme;

  l = inputs.nixpkgs.lib // builtins;
  cfg = config.theme;

  # Single-item list format follows the NixOS options.
  defaultFonts = let
    fonts =
      moduleArgs.osConfig.fonts.fontconfig.defaultFonts
      or {
        monospace = ["DejaVu Sans Mono"];
        sansSerif = ["DejaVu Sans"];
        serif = ["DejaVu Serif"];
        emoji = ["Noto Color Emoji"];
      };
  in
    l.mapAttrs (_: l.head) fonts;

  fontOptionSubmodule = {...}: {
    family = l.mkOption {
      type = str;
    };
    weight = l.mkOption {
      type = int;
      default = 400;
    };
    size = l.mkOption {
      type = int;
      default = 14;
    };
  };
in {
  options.theme = {
    enable = l.mkEnableOption "Whether to enable the theme module.";
    colors = {
      active = l.mkOption {
        description = "Active color scheme.";
        # type = l.types.submodule colorSchemeModule;
        type = l.types.str;
      };
      dark = l.mkOption {
        default = "default-dark";
        description = "Dark color scheme.";
        type = l.types.str;
      };
      light = l.mkOption {
        default = "default-light";
        description = "Light color scheme.";
        type = l.types.str;
      };
    };
    fonts = {
      mono = l.mkOption {
        type = l.types.submodule fontOptionSubmodule;
        default = {
          family = defaultFonts.monospace;
          size = 12;
        };
      };
      term = l.mkOption {
        type = l.types.submodule fontOptionSubmodule;
        default = {
          family = defaultFonts.monospace;
          size = 12;
        };
      };
      sans = l.mkOption {
        type = l.types.submodule fontOptionSubmodule;
        default = {
          family = defaultFonts.sansSerif;
        };
      };
      serif = l.mkOption {
        type = l.types.submodule fontOptionSubmodule;
        default = {
          family = defaultFonts.serif;
          size = 14;
        };
      };
      emoji = l.mkOption {
        type = l.types.submodule fontOptionSubmodule;
        default = {
          family = defaultFonts.emoji;
        };
      };
      symbols = l.mkOption {
        type = l.types.submodule fontOptionSubmodule;
        default = {
          family = "Symbols Nerd Font Mono";
        };
      };
    };
  };

  config = l.mkIf cfg.enable {
    theme.colors.active = l.mkDefault cfg.colors.dark;
    # theme.colors.active = l.mkDefault (let
    #   #: NOTE: Requires `--impure` flag.
    #   envColors = l.getEnv "DOTFIELD_COLORS";
    # in
    #   if (envColors != "")
    #   then (cfg.colors.${envColors} or (getColorScheme envColors))
    #   else cfg.colors.dark);
    colorScheme = getColorScheme cfg.colors.active;
  };
}
