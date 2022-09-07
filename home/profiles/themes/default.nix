{
  config,
  lib,
  pkgs,
  ...
}: let
  # Note that `builtins.getEnv` will only return an empty string unless running
  # an impure build. For that reason, a fallback value is necessary.
  envTheme = builtins.getEnv "DOTFIELD_THEME";
in {
  theme.enable = true;

  theme.colors = {
    active = lib.mkIf (envTheme != "") envTheme;
    dark = "ia-dark";
    light = "ia-light";
  };

  theme.font = {
    mono = {
      family = "Iosevka Xtal";
      size = 13;
    };
    term = {
      family = "Iosevka Xtal Term";
    };
    sans = {
      family = "IBM Plex Sans";
      size = 13;
    };
    serif = {
      family = "IBM Plex Serif";
      size = 13;
    };
    # sym = {
    #   family = "Iosevka Nerd Font Complete";
    #   size = lib.mkDefault config.theme.font.mono.size;
    # };
  };

  home.packages = with pkgs; [
    (writeScriptBin "toggle-dark-mode" (builtins.readFile ./toggle-dark-mode))
  ];

  # https://github.com/nix-community/home-manager/blob/master/modules/misc/specialization.nix#blob-path
  # specialization = [];
}
