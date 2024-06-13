{ config, ... }:
let
  themePolarity = config.theme.color.schemes.default.kind;
in
{
  programs.git.delta.options.light = themePolarity == "light";
}
