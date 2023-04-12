{
  config,
  inputs,
  ...
}: let
  inherit (inputs.apparat.lib.colors) reversePolarity;
in {
  programs.git.difftastic = {
    enable = true;
    background = reversePolarity config.theme.colors.active.kind;
    display = "inline";
  };
}
