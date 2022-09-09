{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (inputs.nix-colors) colorSchemes;
  inherit (lib.eso) inverseSchemeType;
  inherit (config) colorScheme;
in {
  home.packages = with pkgs; [difftastic];
  programs.git.difftastic = {
    enable = lib.mkDefault (!config.programs.git.delta.enable);
    background = inverseSchemeType colorScheme.kind;
  };
}
