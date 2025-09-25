{
  inputs,
  lib,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (inputs.apparat.lib) isEmpty;
  inherit (inputs.apparat.lib.color) derivePolarity hexToRgb;
  inherit (inputs.apparat.lib.color.types) colorHexType;

  rgbColorSubmodule = {
    options = {
      r = mkOption {
        type = types.ints.u8;
        description = "Red channel value";
      };
      g = mkOption {
        type = types.ints.u8;
        description = "Green channel value";
      };
      b = mkOption {
        type = types.ints.u8;
        description = "Blue channel value";
      };
      a = mkOption {
        type = types.nullOr (types.numbersBetween 0 1);
        description = "Alpha channel value as a float between 0.0 and 1.0";
        default = null;
      };
    };
  };

  colorSubmodule =
    { config, ... }:
    {
      options = {
        name = mkOption {
          type = types.nullOr types.str;
          description = "Palette-specific name of the color";
          example = "rosewater";
          default = null;
        };
        hex = mkOption {
          type = colorHexType;
          example = "663399";
        };
        rgb = mkOption {
          type = types.submodule rgbColorSubmodule;
          readOnly = true;
        };
      };

      config = {
        rgb = hexToRgb config.hex;
      };
    };

  paletteSubmodule =
    { config, ... }:
    {
      options = {
        colors = mkOption {
          type = types.attrsOf (types.submodule colorSubmodule);
        };
        by-name = mkOption {
          type = types.attrsOf types.str;
          default = { };
        };
        by-role = mkOption {
          type = types.attrsOf types.str;
          default = { };
        };
      };

      config = {
        by-name =
          config.colors
          |> lib.filterAttrs (_n: v: !isEmpty v.name)
          |> lib.mapAttrs' (_n: v: lib.nameValuePair v.name (lib.removeAttrs v [ "name" ]));
      };
    };

  colorSchemeSubmodule =
    { name, config, ... }:
    {
      options = {
        name = mkOption {
          type = types.str;
          default = name;
          readOnly = true;
        };
        description = mkOption {
          type = types.str;
          default = "";
          example = "A very nice theme";
          description = ''
            Color scheme author
          '';
        };
        author = mkOption {
          type = types.str;
          default = "";
          description = ''
            Color scheme author
          '';
        };
        variant = mkOption {
          type = types.enum [
            "dark"
            "light"
          ];
          default = derivePolarity config.palette.rgb;
          description = ''
            Whether the scheme is dark or light
          '';
        };
        palette = mkOption {
          type = types.submodule paletteSubmodule;
        };
      };
    };
in
{
  options.colorSchemes = mkOption {
    type = types.attrsOf (types.submodule colorSchemeSubmodule);
    default = { };
  };
}
