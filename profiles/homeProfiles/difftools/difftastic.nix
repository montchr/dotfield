{
  programs.git.difftastic = {
    enable = true;
    # FIXME: this shouldn't be necessary (why not base16?)
    # FIXME: should only use this value on desktop!
    # background = reversePolarity config.theme.color.scheme.default.kind;
    display = "inline";
  };
}
