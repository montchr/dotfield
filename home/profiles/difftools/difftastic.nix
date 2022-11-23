{
  config,
  lib,
  pkgs,
  self,
  ...
}: let
  inherit (self.lib) inverseSchemeType;
  inherit (config) colorScheme;
in {
  home.packages = with pkgs; [difftastic];
  programs.git.difftastic = {
    enable = lib.mkDefault (!config.programs.git.delta.enable);
    background = lib.mkIf config.theme.enable (inverseSchemeType colorScheme.kind);
  };
}
