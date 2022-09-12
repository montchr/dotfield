{
  config,
  lib,
  pkgs,
  inputs,
  self,
  ...
}: let
  inherit (self.lib) inverseSchemeType;
  inherit (inputs.nix-colors) colorSchemes;
  inherit (config) colorScheme;
in {
  home.packages = with pkgs; [difftastic];
  programs.git.difftastic = {
    enable = lib.mkDefault (!config.programs.git.delta.enable);
    background = inverseSchemeType colorScheme.kind;
  };
}
