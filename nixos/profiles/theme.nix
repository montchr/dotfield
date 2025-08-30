{
  flake,
  lib,
  pkgs,
  ...
}:
let
  toColorSchemePath = flake.self.lib.theme.toColorSchemePath pkgs;
in
{
  stylix.enable = true;

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
