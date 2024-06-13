{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (flake.inputs.apparat.lib.color) reversePolarity;
  cfg = config.theme;
in
lib.mkIf cfg.enable {
  programs.git.difftastic.background = reversePolarity cfg.color.schemes.default.kind;
}
