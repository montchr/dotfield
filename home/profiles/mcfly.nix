{config, ...}: {
  programs.mcfly = {
    enable = true;
    fuzzySearchFactor = 2;
    keyScheme = "emacs";
    # FIXME: this shouldn't be necessary (why not base16?)
    # FIXME: should only use this value on desktop!
    # enableLightTheme = config.theme.colors.active.kind == "light";
  };
}
