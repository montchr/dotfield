{
  self,
  config,
  pkgs,
  ...
}: let
  inherit (self.lib.colors) inverseSchemeType;
in {
  programs.git.difftastic = {
    enable = true;
    background = inverseSchemeType config.theme.colors.active.kind;
    display = "inline";
  };
}
