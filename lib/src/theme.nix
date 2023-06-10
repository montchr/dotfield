# SPDX-FileCopyrightText: Copyright (c) 2023 Chris Montgomery <chris@cdom.io>
# SPDX-FileCopyrightText: Copyright (c) 2019 Robert Helgesson
# SPDX-License-Identifier: GPL-3.0-or-later OR MIT
{flake, ...}: let
  inherit (flake.inputs) apparat;
  inherit (apparat.lib.color) fromHex;
  l = flake.inputs.nixpkgs.lib // builtins;

  # TODO: prob more useful to expand scope as a general colorscheme getter since
  #       even with this fn it's pretty repetitive
  asHexStrings = l.mapAttrs (_: v: v.hex.r + v.hex.g + v.hex.b);

  /*
  Guesstimate light/dark polarity for a decimal color value.

  ## Types

  RgbDecTriplet  :: { r :: Int, g :: Int, b :: Int }
  SchemePolarity :: "dark" | "light"

  derivePolarity' :: { dec :: RgbDecTriplet, threshold :: Int ? } -> SchemePolarity
  */
  derivePolarity = {
    dec,
    threshold ? 382,
  }: let
    inherit (dec) r g b;
  in
    if r + g + b >= threshold
    then "light"
    else "dark";

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
  mkColor = v: let
    hex = {
      r = l.substring 0 2 v;
      g = l.substring 2 2 v;
      b = l.substring 4 2 v;
    };
    dec = {
      r = fromHex hex.r;
      g = fromHex hex.g;
      b = fromHex hex.b;
    };
  in {inherit hex dec;};

  /*
  Reshape a Base16 color scheme from its canonical form into the shape expected by our theme module.

  ## Types

  mkColorScheme :: { ${n} :: String } -> {
    name :: String,
    colors :: ColorScheme,
    kind :: String
  }
  */
  mkColorScheme = scheme: let
    bases = l.filterAttrs (n: _: l.hasPrefix "base" n) scheme;
    colors = l.mapAttrs (_: mkColor) bases;
  in {
    inherit colors;
    name = l.replaceStrings [" "] ["-"] scheme.scheme;
    kind = derivePolarity {inherit (colors.base00) dec;};
  };
in {
  inherit
    asHexStrings
    derivePolarity
    mkColorScheme
    ;
}
