{ inputs, self, ... }:
{
  aspects.theme.nixos =
    { lib, pkgs, ... }:
    let
      toColorSchemePath = self.lib.theme.toColorSchemePath pkgs;
    in
    {
      imports = [
        inputs.stylix.nixosModules.stylix
      ];

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
    };
}
