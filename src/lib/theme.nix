# SPDX-FileCopyrightText: Copyright (c) 2023 Chris Montgomery <chmont@proton.me>
# SPDX-FileCopyrightText: Copyright (c) 2019 Robert Helgesson
# SPDX-License-Identifier: GPL-3.0-or-later OR MIT
{ lib, inputs, ... }:
let
  inherit (inputs.apparat.lib.color) fromHex;
  inherit (builtins) substring;

  # TODO: prob more useful to expand scope as a general colorscheme getter since
  #       even with this fn it's pretty repetitive
  asHexStrings = lib.mapAttrs (_: v: v.hex.r + v.hex.g + v.hex.b);

  /*
    Guesstimate light/dark polarity for a decimal color value.

    ## Types

    RgbDecTriplet  :: { r :: Int, g :: Int, b :: Int }
    SchemePolarity :: "dark" | "light"

    derivePolarity' :: { dec :: RgbDecTriplet, threshold :: Int ? } -> SchemePolarity
  */
  derivePolarity =
    {
      dec,
      threshold ? 382,
    }:
    let
      inherit (dec) r g b;
    in
    if r + g + b >= threshold then "light" else "dark";

  /*
    ## Types

    ColorScheme :: {
      hex :: {
        r :: String,
        g :: String,
        b :: String,
      },
      dec :: {
        r :: Int,
        g :: Int,
        b :: Int
      }
    }

    mkColor :: String -> ColorScheme
  */
  mkColor =
    v:
    let
      hex = {
        r = substring 0 2 v;
        g = substring 2 2 v;
        b = substring 4 2 v;
      };
      dec = {
        r = fromHex hex.r;
        g = fromHex hex.g;
        b = fromHex hex.b;
      };
    in
    {
      inherit hex dec;
    };

  /*
    Reshape a Base16 color scheme from its canonical form into the shape expected by our theme module.

    FIXME: `name` should be the yaml basename

    ## Types

    mkColorScheme :: { ${n} :: String } -> {
      name :: String,
      colors :: ColorScheme,
      kind :: String
    }
  */
  mkColorScheme =
    scheme:
    let
      bases = lib.filterAttrs (n: _: lib.hasPrefix "base" n) scheme;
      colors = lib.mapAttrs (_: mkColor) bases;
    in
    {
      inherit colors;
      name = builtins.replaceStrings [ " " ] [ "-" ] scheme.scheme;
      kind = derivePolarity { inherit (colors.base00) dec; };
    };

  toColorSchemePath = pkgs: scheme: "${pkgs.base16-schemes}/share/themes/${scheme}.yaml";
in
{
  flake.lib.theme = {
    inherit
      asHexStrings
      derivePolarity
      mkColorScheme
      toColorSchemePath
      ;
  };
}
