{
  config,
  lib,
  inputs,
  self,
  ...
}: let
  inherit (inputs.apparat.lib.color) reversePolarity;
  inherit (self.lib.colors) getColorScheme;
  inherit (config) theme;
in {
  programs.git.difftastic = {
    enable = true;
    background = lib.mkIf theme.enable (reversePolarity
      (getColorScheme
        config.theme.colors.active)
      .kind);
    display = "inline";
  };
}
