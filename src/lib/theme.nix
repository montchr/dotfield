# SPDX-FileCopyrightText: Copyright (c) 2023 Chris Montgomery <chmont@proton.me>
# SPDX-FileCopyrightText: Copyright (c) 2019 Robert Helgesson
# SPDX-License-Identifier: GPL-3.0-or-later OR MIT
{ lib, inputs, ... }:
let
  inherit (inputs.apparat.lib.color) derivePolarity fromHex;

  # TODO: prob more useful to expand scope as a general colorscheme getter since
  #       even with this fn it's pretty repetitive
  asHexStrings = lib.mapAttrs (_: v: v.hex.r + v.hex.g + v.hex.b);

  /*
    # Type

    ```
    mkColor :: String -> {
      hex :: String,
      dec :: {
        r :: Int,
        g :: Int,
        b :: Int
      }
    }
    ```
  */
  mkColor = v: {
    hex = v;
    rgb = {
      r = fromHex lib.substring 0 2 v;
      g = fromHex lib.substring 2 2 v;
      b = fromHex lib.substring 4 2 v;
    };
  };

  /*
    Reshape a Base16 color scheme from its canonical form into the shape expected by our theme module.

    ## Types

    mkColorScheme :: { ${n} :: String } -> {
      name :: String,
      colors :: ColorScheme,
      variant :: String
    }
  */
  mkColorScheme =
    scheme:
    let
      bases = lib.filterAttrs (n: _: lib.hasPrefix "base" n) scheme;
      palette = lib.mapAttrs (_: mkColor) bases;
    in
    {
      inherit palette;
      # FIXME: `name` should be the yaml basename
      name = lib.replaceStrings [ " " ] [ "-" ] scheme.scheme;
      variant = derivePolarity palette.base00.rgb;
    };

  toColorSchemePath = pkgs: scheme: "${pkgs.base16-schemes}/share/themes/${scheme}.yaml";
in
{
  flake.lib.theme = {
    inherit
      asHexStrings
      mkColorScheme
      toColorSchemePath
      ;
  };
}
