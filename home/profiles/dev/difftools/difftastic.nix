{...}:
# let
#   inherit (inputs.apparat.lib.color) reversePolarity;
# in
{
  programs.git.difftastic = {
    enable = true;
    # FIXME: this shouldn't be necessary (why not base16?)
    # FIXME: should only use this value on desktop!
    # background = reversePolarity config.theme.colors.active.kind;
    display = "inline";
  };
}
