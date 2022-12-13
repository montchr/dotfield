{
  config,
  inputs,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
  cfg = config.theme;
  getColorScheme = name: inputs.nix-colors.colorSchemes.${name};

  #: NOTE: Requires `--impure` flag.
  envColors = l.getEnv "DOTFIELD_COLORS";
  envColorsLight = l.getEnv "DOTFIELD_COLORS_LIGHT";
  envColorsDark = l.getEnv "DOTFIELD_COLORS_DARK";

  dark =
    if (envColorsDark != "")
    then envColorsDark
    else "black-metal-khold";
  light =
    if (envColorsLight != "")
    then envColorsLight
    else "one-light";

  sessionVariables = {
    DOTFIELD_COLORS = cfg.colors.active.slug;
    DOTFIELD_COLORS_DARK = cfg.colors.dark.slug;
    DOTFIELD_COLORS_LIGHT = cfg.colors.light.slug;
  };
in {
  theme.enable = true;
  theme.colors = {
    active = l.mkIf (envColors != "") (getColorScheme envColors);
    dark = getColorScheme dark;
    light = getColorScheme light;
  };
  theme.fonts = {
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
  # specialization = [];
}
