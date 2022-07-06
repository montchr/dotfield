{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  # Note that `builtins.getEnv` will only return an empty string unless running
  # an impure build. For that reason, a fallback value is necessary.
  envTheme = builtins.getEnv "DOTFIELD_THEME";
  theme =
    if envTheme != ""
    then envTheme
    else "black-metal-khold";
in {
  colorscheme = inputs.nix-colors.colorSchemes.${theme};

  theme.font = {
    mono = {
      family = "Iosevka Seadome";
      size = 13;
    };
    sans = {
      family = "Iosevka Comfy Duo";
      size = 13;
    };
    serif = {
      family = "Iosevka Curly Slab";
      size = 13;
    };
    # sym = {
    #   family = "Iosevka Nerd Font Complete";
    #   size = lib.mkDefault config.theme.font.mono.size;
    # };
  };

  home.sessionVariables = {
    BASE16_THEME_DARK = "black-metal-khold";
    BASE16_THEME_LIGHT = "grayscale-light";
    DOTFIELD_EMACS_THEME_DARK = "modus-vivendi";
    DOTFIELD_EMACS_THEME_LIGHT = "modus-operandi";
  };

  home.packages = with pkgs; [
    (writeScriptBin "toggle-dark-mode" (builtins.readFile ./toggle-dark-mode))
  ];

  # https://github.com/nix-community/home-manager/blob/master/modules/misc/specialization.nix#blob-path
  # specialization = [];
}
