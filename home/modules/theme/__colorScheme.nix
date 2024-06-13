# SPDX-FileCopyrightText: Copyright (c) 2023 Chris Montgomery <chmont@proton.me>
# SPDX-FileCopyrightText: Copyright (c) 2019-2023 Robert Helgesson
# SPDX-License-Identifier: GPL-3.0-or-later OR MIT
# Source: <https://gitlab.com/rycee/nur-expressions/-/blob/e62185746f8758e11a2fbca1e08e025f2b57e726/hm-modules/theme-base16/default.nix>
{ flake }:
{ config, ... }:
let
  inherit (flake.lib.theme) derivePolarity;
  inherit (flake.inputs) apparat;
  inherit (apparat.lib.color) fromHex toHex2;
  inherit (apparat.types.color) rgbChannelDec rgbChannelHex rgbHex;
  l = flake.inputs.nixpkgs.lib // builtins;

  mkHexColorOption =
    component:
    l.mkOption {
      type = rgbChannelHex;
      example = "b1";
      visible = false;
      description = "The ${component} component value as a hexadecimal string.";
    };

  mkDecColorOption =
    component:
    l.mkOption {
      type = rgbChannelDec;
      example = 177;
      visible = false;
      description = "The ${component} component value as a hexadecimal string.";
    };

  colorModule = l.types.submodule (
    { config, ... }:
    {
      options = {
        hex.r = mkHexColorOption "red";
        hex.g = mkHexColorOption "green";
        hex.b = mkHexColorOption "blue";
        hex.rgb = l.mkOption {
          type = rgbHex;
          readOnly = true;
          visible = false;
          description = "Concatenated hexadecimal string.";
        };

        dec.r = mkDecColorOption "red";
        dec.g = mkDecColorOption "green";
        dec.b = mkDecColorOption "blue";
      };

      config = {
        hex.r = l.mkDefault (toHex2 config.dec.r);
        hex.g = l.mkDefault (toHex2 config.dec.g);
        hex.b = l.mkDefault (toHex2 config.dec.b);
        hex.rgb = config.hex.r + config.hex.g + config.hex.b;

        dec.r = l.mkDefault (fromHex config.hex.r);
        dec.g = l.mkDefault (fromHex config.hex.g);
        dec.b = l.mkDefault (fromHex config.hex.b);
      };
    }
  );
in
{
  options = {
    name = l.mkOption {
      type = l.types.str;
      example = "Solarized Dark";
      description = ''
        The theme name.
      '';
    };

    author = l.mkOption {
      type = l.types.str;
      example = "Ethan Schoonover";
      description = ''
        The theme author.
      '';
    };

    kind = l.mkOption {
      type = l.types.enum [
        "dark"
        "light"
      ];
      example = "light";
      default = derivePolarity {
        inherit (config.colors.base00) dec;
        threshold = 382;
      };
      defaultText = l.literalExpression ''
        "light", if sum of RGB components of base00 color ≥ 382,
        "dark", otherwise
      '';
      description = ''
        Whether theme is dark or light. The default value is determined by a
        basic heuristic, if an incorrect value is found then this option must
        be set explicitly.
      '';
    };

    colors =
      let
        mkHexColorOption = l.mkOption {
          type = colorModule;
          example = {
            dec = {
              r = 177;
              g = 42;
              b = 42;
            };
          };
          description = ''
            Color value. Either a hexadecimal or decimal RGB triplet must be
            given. If a hexadecimal triplet is given then the decimal triplet is
            automatically populated, and vice versa. That is, the example could
            be equivalently written:

            ```
            { hex.r = "b1"; hex.g = "2a"; hex.b = "2a"; }
            ```

            In addition, note the following example and its expansion:

            ```
            "red dec: ''${dec.r}, red hex: ''${hex.r}, rgb hex: ''${hex.rgb}"
            ```

            > red dec: 177, red hex: b1, rgb hex: b12a2a
          '';
        };
      in
      {
        base00 = mkHexColorOption;
        base01 = mkHexColorOption;
        base02 = mkHexColorOption;
        base03 = mkHexColorOption;
        base04 = mkHexColorOption;
        base05 = mkHexColorOption;
        base06 = mkHexColorOption;
        base07 = mkHexColorOption;
        base08 = mkHexColorOption;
        base09 = mkHexColorOption;
        base0A = mkHexColorOption;
        base0B = mkHexColorOption;
        base0C = mkHexColorOption;
        base0D = mkHexColorOption;
        base0E = mkHexColorOption;
        base0F = mkHexColorOption;
      };
  };
}
