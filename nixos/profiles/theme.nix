{
  flake,
  lib,
  config,
  pkgs,
  ...
}:
let
  toColorSchemePath = scheme: "${pkgs.base16-schemes}/share/themes/${scheme}.yaml";
in
{
  stylix.base16Scheme = lib.mkDefault (toColorSchemePath "catppuccin-mocha");

  specialisation = {
    dark.configuration = {
      stylix.base16Scheme = toColorSchemePath "catppuccin-mocha";
    };
    light.configuration = {
      stylix.base16Scheme = toColorSchemePath "catppuccin-latte";
    };
  };
}
