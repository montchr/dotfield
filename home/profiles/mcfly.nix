{config, ...}: {
  programs.mcfly = {
    enable = true;
    fuzzySearchFactor = 2;
    keyScheme = "emacs";
    enableLightTheme = config.theme.colors.active.kind == "light";
  };
}
