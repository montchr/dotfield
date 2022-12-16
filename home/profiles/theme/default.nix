{
  config,
  inputs,
  self,
  ...
}: let
  inherit (self.lib.colors) getColorScheme;
  l = inputs.nixpkgs.lib // builtins;
  cfg = config.theme;

  #: NOTE: Requires `--impure` flag.
  envColors = l.getEnv "DOTFIELD_COLORS";

  colors = {
    active =
      if (envColors != "")
      then (colors.${envColors} or (getColorScheme envColors))
      else colors.dark;
    dark = getColorScheme "black-metal-khold";
    light = getColorScheme "one-light";
  };

  sessionVariables = {
    # DOTFIELD_THEME_MODE = cfg.colors.active.kind;
    DOTFIELD_COLORS = cfg.colors.active.slug;
    DOTFIELD_COLORS_DARK = cfg.colors.dark.slug;
    DOTFIELD_COLORS_LIGHT = cfg.colors.light.slug;
  };
in {
  theme = {
    enable = true;
    inherit colors;
    fonts = {
      mono = {
        family = "Iosevka";
        size = 13;
      };
      term = {
        family = "Iosevka Term";
      };
      sans = {
        family = "IBM Plex Sans";
        size = 13;
      };
      serif = {
        family = "IBM Plex Serif";
        size = 13;
      };
      symbols = {
        family = "Symbols Nerd Font Mono";
      };
    };
  };

  home = {inherit sessionVariables;};
  programs.bash = {inherit sessionVariables;};
  programs.zsh = {inherit sessionVariables;};

  programs.kitty = {
    settings = rec {
      font_family = "Iosevka Term";
      bold_font = "${font_family} Semibold";
      italic_font = "${font_family} Italic";
      bold_italic_font = "${font_family} Semibold Italic";
    };
    extraConfig = let
      psName = "Iosevka-Term";
    in ''
      font_features ${psName}                  -calt +dlig
      font_features ${psName}-Semibold         -calt +dlig
      font_features ${psName}-Italic           -calt +dlig
      font_features ${psName}-Semibold-Italic  -calt +dlig
    '';
  };

  # https://github.com/nix-community/home-manager/blob/master/modules/misc/specialization.nix#blob-path
  # specialization = {
  #   # dark.configuration = {
  #   #   theme.colors.active = l.mkForce config.theme.colors.dark;
  #   # };
  #   light.configuration = {
  #     theme.colors.active = l.mkForce colors.light;
  #     colorScheme = l.mkForce colors.light;
  #   };
  # };
}