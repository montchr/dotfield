{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (flake.inputs.apparat.lib.color) reversePolarity;
  cfg = config.programs.git.difftastic;
  colorScheme = config.theme.color.schemes.default;
in
lib.mkIf cfg.enable { programs.git.difftastic.background = reversePolarity colorScheme.kind; }
