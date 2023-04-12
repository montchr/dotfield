{inputs, ...}: let
  inherit (inputs.nix-colors) colorSchemes;
  inherit (inputs.apparat.lib.colors) withHexPrefixes;

  /*
  Returns a library color scheme specification.

  Type: getColorScheme :: string -> AttrSet
  */
  getColorScheme = name: colorSchemes.${name};

  /*
  Returns a set of prefixed hex color values for a library color scheme.

  TODO: IIRC this feature is now in upstream.

  Type: getColorScheme :: string -> AttrSet
  */
  getColorScheme' = name: withHexPrefixes (getColorScheme name).colors;
in {
  inherit
    getColorScheme
    getColorScheme'
    ;
}
