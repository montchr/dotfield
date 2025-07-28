{
  lib,
  self,
  inputs,
  ...
}:
let
  inherit (self.lib.theme) toColorSchemePath;
in

{
  dotfield.modules.defaults.nixos = {
    imports = [ self.dotfield.modules.theme.nixos ];
  };

  dotfield.modules.theme.nixos =
    { pkgs, ... }:
    let
      toColorSchemePath' = toColorSchemePath pkgs;
    in
    {
      imports = [ inputs.stylix.nixosModules.stylix ];

      stylix.enable = true;
      stylix.base16Scheme = lib.mkDefault (toColorSchemePath' "catppuccin-mocha");

      specialisation = {
        dark.configuration = {
          stylix.base16Scheme = toColorSchemePath' "catppuccin-mocha";
        };
        light.configuration = {
          stylix.base16Scheme = toColorSchemePath' "catppuccin-latte";
        };
      };

    };
}
